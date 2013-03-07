// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
// 
// AceWiki is free software: you can redistribute it and/or modify it under the terms of the GNU
// Lesser General Public License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
// 
// AceWiki is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License along with AceWiki. If
// not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.chartparser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ch.uzh.ifi.attempto.base.PredictiveParser;

/**
 * This is a chart parser (concretely an Earley parser) that implements the predictive parser
 * interface.
 * 
 * @see Grammar
 * @author Tobias Kuhn
 */
public class ChartParser implements PredictiveParser {
	
	private final org.slf4j.Logger log = org.slf4j.LoggerFactory.getLogger(this.getClass());
	
	private final Grammar grammar;
	private final String startCategoryName;
	private final Nonterminal[] context;
	private DynamicLexicon dynLexicon;
	private final Chart chart;
	private final List<String> tokens = new ArrayList<String>();
	private final List<CPNextTokenOptions> options = new ArrayList<CPNextTokenOptions>();
	private final List<List<FeatureMap>> backwardReferences = new ArrayList<List<FeatureMap>>();
	private ParseTree parseTree;
	private Map<String, Integer> progressTable;
	private boolean recalculateParseTree = true;
	private String positionIdentifierPrefix = "#";
	private boolean debug;
	
	/**
	 * Creates a new chart parser for the given grammar. The grammar must not be changed afterwards.
	 * 
	 * @param grammar The grammar to be used by the chart parser.
	 * @param startCategoryName The name of the start category.
	 * @param context A list of forward references and scope openers that define the context.
	 */
	public ChartParser(Grammar grammar, String startCategoryName, List<Nonterminal> context) {
		this.grammar = grammar;
		this.startCategoryName = startCategoryName;
		if (context == null) {
			this.context = new Nonterminal[0];
		} else {
			this.context = context.toArray(new Nonterminal[0]);
		}
		this.chart = new Chart(grammar);
		options.add(null);
		init();
		runParsingSteps();
	}
	
	/**
	 * Creates a new chart parser for the given grammar. The grammar must not be changed afterwards.
	 * 
	 * @param grammar The grammar to be used by the chart parser.
	 * @param startCategoryName The name of the start category.
	 */
	public ChartParser(Grammar grammar, String startCategoryName) {
		this(grammar, startCategoryName, null);
	}
	
	/**
	 * This method can be used to switch on/off debug mode (default is off). In debug mode, messages
	 * about the actions of the chart parser are printed onto the standard error device.
	 * 
	 * @param debug true to switch debug mode on, or false to switch it off.
	 */
	public void debug(boolean debug) {
		this.debug = debug;
	}
	
	/**
	 * Sets the dynamic lexicon.
	 * 
	 * @param dynLexicon The dynamic lexicon.
	 */
	public void setDynamicLexicon(DynamicLexicon dynLexicon) {
		this.dynLexicon = dynLexicon;
		updateConcreteOptions(tokens.size());
	}
	
	/**
	 * Sets the prefix for the position identifiers that are assigned to the variables of the
	 * position operator "#". The default prefix is "#" so that the position identifiers are "#0",
	 * "#1", "#2" and so on.
	 * 
	 * @param prefix The new prefix.
	 */
	public void setPositionIdentifierPrefix(String prefix) {
		this.positionIdentifierPrefix = prefix.toString();
	}
	
	public void addToken(String token) {
		chart.addEdge(new Edge(tokens.size(), new Terminal(token)));
		
		List<LexicalRule> lexRules;
		if (dynLexicon == null) {
			lexRules = grammar.lexRulesByWord(token);
		} else {
			lexRules = new ArrayList<LexicalRule>();
			lexRules.addAll(grammar.lexRulesByWord(token));
			lexRules.addAll(dynLexicon.getLexRules(token));
		}
		
		// add edges for applicable lexical rules:
		for (LexicalRule lexRule : lexRules) {
			Edge edge = new Edge(tokens.size(), lexRule.deepCopy());
			chart.addEdge(edge);
			if (debug) log.debug("SCANNER: {}", edge);
		}
		
		runParsingSteps();

		// add the token to the list of tokens:
		tokens.add(token);
		if (debug) {
			log.debug("ADD TOKEN: {}", token);
			log.debug("TOKEN LIST: {}", tokens);
		}
		options.add(null);
		backwardReferences.add(new ArrayList<FeatureMap>());
		progressTable = null;
		
		runParsingSteps();
		//if (debug) log("CHART:");
		//if (debug) log(chart);
		recalculateParseTree = true;
	}
	
	public void addTokens(List<String> tokens) {
		for (String t : tokens) {
			addToken(t);
		}
	}
	
	public void removeToken() {
		chart.removeEdgesWithEndPos(tokens.size());
		backwardReferences.remove(tokens.size()-1);
		options.remove(tokens.size());
		tokens.remove(tokens.size()-1);
		progressTable = null;
		updateConcreteOptions(tokens.size());
		recalculateParseTree = true;
		if (debug) {
			log.debug("REMOVE LAST TOKEN.");
			log.debug("TOKEN LIST: {}", tokens);
		}
	}
	
	public void removeAllTokens() {
		if (debug) log.debug("REMOVE ALL TOKENS");
		tokens.clear();
		options.clear();
		options.add(null);
		backwardReferences.clear();
		progressTable = null;
		chart.clear();
		init();
		runParsingSteps();
		recalculateParseTree = true;
	}
	
	public void setTokens(List<String> tokens) {
		removeAllTokens();
		addTokens(tokens);
	}
	
	public List<String> getTokens() {
		return new ArrayList<String>(tokens);
	}
	
	public int getTokenCount() {
		return tokens.size();
	}

	/**
	 * This method returns the token number to which the token at the given position refers, if it
	 * is a reference. -1 is returned if the given token is not a reference.
	 * 
	 * @param pos The position of the token for which the reference should be returned.
	 * @return The token number to which the token at the given position refers, or -1.
	 */
	public int getReference(int pos) {
		int ref = -1;
		for (FeatureMap f : getBackwardReferences(pos)) {
			String s = f.getFeature("*pos").getString();
			if (s != null) {
				int i = new Integer(s) - 1;
				if (i > -1) {
					ref = i;
				}
			}
		}
		return ref;
	}
	
	public int getReference() {
		return getReference(tokens.size()-1);
	}
	
	/**
	 * Return a list of feature maps that show how the backward references at the given position
	 * in the text can be resolved. These feature maps contain special features of the form "*pos"
	 * that denote the textual position of the respective forward references.
	 * 
	 * @param pos The position of the backward reference.
	 * @return The list of feature maps.
	 */
	public List<FeatureMap> getBackwardReferences(int pos) {
		if (pos == -1 || pos >= tokens.size()) {
			return new ArrayList<FeatureMap>();
		}
		return backwardReferences.get(pos);
	}
	
	/**
	 * Returns a list of feature maps that show how the backward references at the end of the
	 * token sequence can be resolved.
	 * 
	 * @return The list of feature maps.
	 */
	public List<FeatureMap> getBackwardReferences() {
		return getBackwardReferences(tokens.size()-1);
	}
	
	public boolean isComplete() {
		for (Edge e : chart.getEdgesByEndPos(tokens.size())) {
			if (e.getStartPos() != 0) continue;
			if (e.isActive()) continue;
			if (!e.getHead().getName().equals(startCategoryName)) continue;
			return true;
		}
		return false;
	}
	
	/**
	 * Returns the parse tree of the parsed text if it is a complete statement according to the
	 * given grammar and category. Null is returned if the text is not a complete statement.
	 * 
	 * @param categoryName The category name.
	 * @return The parse tree.
	 */
	public ParseTree getParseTree(String categoryName) {
		for (Edge e : chart.getEdgesByEndPos(tokens.size())) {
			if (e.getStartPos() != 0) continue;
			if (e.isActive()) continue;
			if (!e.getHead().getName().equals(categoryName)) continue;
			return new ParseTree(e);
		}
		return null;
	}
	
	/**
	 * Returns the parse tree of the parsed text if it is a complete statement according to the
	 * given grammar and start category. Null is returned if the text is not a complete statement.
	 * 
	 * @return The parse tree.
	 */
	public ParseTree getParseTree() {
		if (recalculateParseTree) {
			parseTree = getParseTree(startCategoryName);
		}
		recalculateParseTree = false;
		return parseTree;
	}
	
	/**
	 * This methods shows the possible tokens that could be used to continue the text at the given
	 * position.
	 * 
	 * @param position The position at which the possible next tokens should be found.
	 * @return The options describing the possible next tokens.
	 */
	public CPNextTokenOptions getNextTokenOptions(int position) {
		createOptions(position);
		return options.get(position);
	}
	
	public CPNextTokenOptions getNextTokenOptions() {
		return getNextTokenOptions(tokens.size());
	}
	
	/**
	 * This method returns a set of abstract options describing the possible next tokens at the
	 * given position in an abstract way.
	 * 
	 * @param position The position at which the possible next tokens should be found.
	 * @return The set of abstract options describing the possible next tokens.
	 */
	public Set<CPAbstractOption> getAbstractOptions(int position) {
		createOptions(position);
		return options.get(position).getAbstractOptions();
	}
	
	/**
	 * This method returns a set of abstract options describing the possible next tokens at the end
	 * position in an abstract way.
	 * 
	 * @return The set of abstract options describing the possible next tokens.
	 */
	public Set<CPAbstractOption> getAbstractOptions() {
		return getAbstractOptions(tokens.size());
	}

	/**
	 * This method returns a set of concrete options describing the possible next tokens at the
	 * given position in a concrete way.
	 * 
	 * @param position The position at which the possible next tokens should be found.
	 * @return The set of concrete options describing the possible next tokens.
	 */
	public Set<CPConcreteOption> getConcreteOptions(int position) {
		createOptions(position);
		return options.get(position).getConcreteOptions();
	}

	/**
	 * This method returns a set of concrete options describing the possible next tokens at the end
	 * position in a concrete way.
	 * 
	 * @return The set of concrete options describing the possible next tokens.
	 */
	public Set<CPConcreteOption> getConcreteOptions() {
		return getConcreteOptions(tokens.size());
	}
	
	public boolean isPossibleNextToken(String token) {
		if (getNextTokenOptions().containsToken(token)) return true;
		for (LexicalRule lr : dynLexicon.getLexRules(token)) {
			if (!lr.getWord().getName().equals(token)) continue;
			if (getNextTokenOptions().containsCategory(lr.getCategory())) return true;
		}
		return false;
	}
	
	/**
	 * Creates the abstract and concrete options at the given position. The options are cached.
	 * 
	 * @param position The position for which the options should be calculated.
	 */
	private void createOptions(int position) {
		if (options.get(position) == null) {
			Set<CPAbstractOption> aOptions = createAbstractOptions(position);
			Set<CPConcreteOption> cOptions = createConcreteOptions(position, aOptions);
			options.set(position, new CPNextTokenOptions(aOptions, cOptions));
		}
	}
	
	private void updateConcreteOptions(int position) {
		if (options.get(position) == null) {
			createOptions(position);
		} else {
			Set<CPAbstractOption> aOptions = options.get(position).getAbstractOptions();
			Set<CPConcreteOption> cOptions = createConcreteOptions(position, aOptions);
			options.set(position, new CPNextTokenOptions(aOptions, cOptions));
		}
	}
	
	/**
	 * Calculates the set of abstract options for the given position.
	 * 
	 * @param position The position for which the abstract options should be calculated.
	 * @return The set of abstract options.
	 */
	private Set<CPAbstractOption> createAbstractOptions(int position) {
		Set<CPAbstractOption> aOptions = new HashSet<CPAbstractOption>();
		for (Edge e : chart.getEdgesByEndPos(position)) {
			if (!e.isActive()) continue;
			if (e.getNextActive() instanceof Nonterminal) continue;
			
			BackrefCategory backref = null;
			Nonterminal negbackref = null;
			int refpos = 0;
			Category[] body = e.getBody();
			int p = e.getProgress();
			for (int i = p + 1 ; i < body.length ; i++) {
				Category c = body[i];
				if (!(c instanceof Nonterminal)) continue;
				if (c instanceof BackrefCategory) {
					backref = (BackrefCategory) c;
					refpos = i;
				} else if (i == (p+1) && c.getName().equals("/<")) {
					negbackref = (Nonterminal) c;
					refpos = i;
				}
				break;
			}
			
			if (backref != null) {
				// For edges with backwards references, the possible bindings have to be performed:
				for (int i = e.getCombinedAnteList().length - 1 ; i >= 0 ; i--) {
					if (e.getCombinedAnteList()[i].getName().equals("//")) continue;
					
					int posrefsCount = backref.getPosFeatureMaps().size();
					int negrefsCount = backref.getNegFeatureMaps().size();
					List<Category> exceptions = null;
					boolean makeRestriction = true;
					
					if (refpos == (p+1)) {
						exceptions = new ArrayList<Category>();
						for (int j = 0 ; j < negrefsCount ; j++) {
							Edge eC = e.deepCopy();
							try {
								FeatureMap backrefFm =
									((BackrefCategory) eC.getBody()[refpos]).getNegFeatureMaps().get(j);
								eC.getCombinedAnteList()[i].getFeatureMap().unify(backrefFm);
								if (eC.getNextActive() instanceof Terminal) {
									makeRestriction = false;
									break;
								} else {
									exceptions.add(eC.getNextActive());
								}
							} catch (UnificationFailedException ex) {}
						}
					}
					if (!makeRestriction) break;
					
					for (int j = 0 ; j < posrefsCount ; j++) {
						Edge eC = e.deepCopy();
						try {
							FeatureMap backrefFm =
								((BackrefCategory) eC.getBody()[refpos]).getPosFeatureMaps().get(j);
							eC.getCombinedAnteList()[i].getFeatureMap().unify(backrefFm);
							if (exceptions != null) {
								aOptions.add(new CPAbstractOption(
										grammar,
										eC.getNextActive(),
										copyExceptionsList(exceptions)
									));
							} else {
								aOptions.add(new CPAbstractOption(grammar, eC.getNextActive()));
							}
						} catch (UnificationFailedException ex) {}
					}
				}
			} else if (negbackref != null) {
				List<Category> exceptions = new ArrayList<Category>();
				// Edges with negative backwards references lead to exceptions:
				boolean makeRestriction = true;
				for (int i = 0 ; i < e.getCombinedAnteList().length ; i++) {
					if (e.getCombinedAnteList()[i].getName().equals("//")) continue;
					Edge eC = e.deepCopy();
					try {
						eC.getCombinedAnteList()[i].getFeatureMap().unify(eC.getBody()[refpos].getFeatureMap());
						if (eC.getNextActive() instanceof Terminal) {
							makeRestriction = false;
							break;
						} else {
							exceptions.add(eC.getNextActive());
						}
					} catch (UnificationFailedException ex) {}
				}
				if (makeRestriction) {
					aOptions.add(new CPAbstractOption(grammar, e.getNextActive().deepCopy(), exceptions));
				}
			} else {
				aOptions.add(new CPAbstractOption(grammar, e.getNextActive().deepCopy()));
			}
		}
		if (debug) {
			for (CPAbstractOption o : aOptions) {
				log.debug("LOOKING FORWARD: {}", o);
			}
		}
		
		return aOptions;
	}

	/**
	 * Calculates the set of concrete options for the given position on the basis of a set of
	 * abstract options.
	 * 
	 * @param position The position for which the concrete options should be calculated.
	 * @param aOptions The set of abstract options.
	 * @return The set of concrete options.
	 */
	private Set<CPConcreteOption> createConcreteOptions(int position, Set<CPAbstractOption> aOptions) {
		Set<CPConcreteOption> cOptions = new HashSet<CPConcreteOption>();
		
		for (CPAbstractOption ao : aOptions) {
			if (ao.getCategory() instanceof Preterminal) {

				List<LexicalRule> lexRules;
				if (dynLexicon == null) {
					lexRules = grammar.lexRulesByCat(ao.getCategory().getName());
				} else {
					lexRules = new ArrayList<LexicalRule>();
					lexRules.addAll(grammar.lexRulesByCat(ao.getCategory().getName()));
					lexRules.addAll(dynLexicon.getLexRules(ao));
				}
				
				for (LexicalRule lexRule : lexRules) {
					if (ao.isFulfilledBy(lexRule.getCategory())) {
						cOptions.add(new CPConcreteOption(grammar, lexRule.deepCopy()));
					}
				}
			} else if (ao.getCategory() instanceof Terminal) {
				cOptions.add(new CPConcreteOption(grammar, (Terminal) ao.getCategory(), null));
			}
		}
		
		return cOptions;
	}
	
	/**
	 * Runs the initialization step of the Earley parsing algorithm.
	 */
	private void init() {
		for (GrammarRule rule : grammar.rulesByHeadName(startCategoryName)) {
			Edge edge = new Edge(0, rule.deepCopy(), context);
			chart.addEdge(edge);
			if (debug) log.debug("INIT: {}  --->  {}", rule, edge);
		}
	}
	
	/**
	 * Runs the main parsing steps of the Earley algorithm. These parsing steps consists of the
	 * completion/prediction/resolution loop.
	 */
	private void runParsingSteps() {
		// Run completion/predition/resolution until neither of them generates a new edge:
		int chartSize = 0;
		int step = 0;
		int idleSteps = 0;
		if (progressTable == null) {
			progressTable = new HashMap<String, Integer>();
			progressTable.put("prediction", 0);
			progressTable.put("completion", 0);
			progressTable.put("resolution", 0);
		}
		while (true) {
			step++;
			chartSize = chart.getSize();
			if (step == 1) {
				predict(progressTable);
			} else if (step == 2) {
				resolve(progressTable);
			} else {
				complete(progressTable);
				step = 0;
			}
			if (chartSize == chart.getSize()) {
				idleSteps++;
			} else {
				idleSteps = 0;
			}
			if (idleSteps > 2) {
				break;
			}
		}
	}
	
	/**
	 * Runs the prediction step of the Earley parsing algorithm.
	 * 
	 * @param progressTable This table captures the progress state in order to prevent from
	 *     checking the same edges more than once.
	 */
	private void predict(Map<String, Integer> progressTable) {
		List<Edge> l = chart.getEdgesByEndPos(tokens.size());
		for (int i = new Integer(progressTable.get("prediction")) ; i < l.size() ; i++) {
			// During this loop, elements might be added to the end of the list l.
			
			Edge existingEdge = l.get(i);
			Category category = existingEdge.getNextActive();
			if (category == null) continue;
			if (category instanceof Terminal) continue;
			if (category.isSpecialCategory()) continue;
			if (debug) log.debug("PREDICTION FOR CATEGORY: {}", category);
			
			for (GrammarRule rule : grammar.rulesByHeadName(category.getName())) {
				try {
					if (!category.isSimilar(rule.getHead())) continue;
					Edge edgeC = existingEdge.deepCopy();
					GrammarRule ruleC = rule.deepCopy();
					edgeC.getNextActive().unify(ruleC.getHead());
					Edge edge = new Edge(tokens.size(), ruleC, edgeC.getCombinedAnteList());
					boolean isNewEdge = chart.addEdge(edge);
					if (debug) log.debug("PREDICT ({}): {}  --->  {}", (isNewEdge ? "NEW" : "KNOWN"), rule, edge);
				} catch (UnificationFailedException ex) {
					continue;
				}
			}
		}
		progressTable.put("prediction", l.size());
	}
	
	/**
	 * Runs the completion step of the Earley parsing algorithm.
	 * 
	 * @param progressTable This table captures the progress state in order to prevent from
	 *     checking the same edges more than once.
	 */
	private void complete(Map<String, Integer> progressTable) {
		List<Edge> l1 = chart.getEdgesByEndPos(tokens.size());
		for (int i1 = 0 ; i1 < l1.size() ; i1++) {
			// During this loop, elements might be added to the end of the list l1.
			
			Edge passiveEdge = l1.get(i1);
			if (passiveEdge.isActive()) continue;
			if (debug) log.debug("COMPLETION FOR EDGE: {}", passiveEdge);

			List<Edge> l2 = chart.getEdgesByEndPos(passiveEdge.getStartPos());
			int start;
			if (i1 < progressTable.get("completion")) {
				Integer progress = progressTable.get("completion " + i1);
				if (progress == null) {
					start = 0;
				} else {
					start = progress;
				}
			} else {
				start = 0;
			}
			
			for (int i2 = start ; i2 < l2.size() ; i2++) {
				// During this loop, elements might be added to the end of the list l2.
				
				Edge edge = l2.get(i2);
				if (!edge.isActive()) continue;
				if (!edge.getNextActive().getName().equals(passiveEdge.getHead().getName())) continue;
				
				try {
					if (!passiveEdge.getHead().isSimilar(edge.getNextActive())) continue;
					Edge passiveEdgeC = passiveEdge.deepCopy();
					Edge edgeC = edge.deepCopy();
					passiveEdgeC.getHead().unify(edgeC.getNextActive());
					if (!passiveEdge.carriesAntecedentInformation()) {
						// Antecedent lists have to match:
						Category[] al1 = edgeC.getCombinedAnteList();
						Category[] al2 = passiveEdgeC.getExternalAnteList();
						if (al1.length != al2.length) throw new UnificationFailedException();
						for (int i = 0 ; i < al1.length ; i++) {
							al1[i].unify(al2[i]);
						}
					}
					edgeC.step(tokens.size(), passiveEdgeC);
					boolean isNewEdge = chart.addEdge(edgeC);
					if (debug) log.debug("COMPLETE ({}): {}  --->  {}", (isNewEdge ? "NEW" : "KNOWN"), edge, edgeC);
				} catch (UnificationFailedException ex) {
					continue;
				}
			}
			progressTable.put("completion " + i1, l2.size());
		}
		progressTable.put("completion", l1.size());
	}

	/**
	 * Runs the resolution step, which is an extension of the standard Earley algorithm.
	 * 
	 * @param progressTable This table captures the progress state in order to prevent from
	 *     checking the same edges more than once.
	 */
	private void resolve(Map<String, Integer> progressTable) {
		List<Edge> l1 = chart.getEdgesByEndPos(tokens.size());
		for (int i1 = progressTable.get("resolution") ; i1 < l1.size() ; i1++) {
			// During this loop, elements might be added to the end of the list l1.
			
			Edge edge = l1.get(i1);
			if (!edge.isActive()) continue;
			
			String n = edge.getNextActive().getName();
			List<Edge> newEdges = new ArrayList<Edge>();
			if (n.equals("#")) {
				Edge edgeC = edge.deepCopy();
				try {
					String posId = positionIdentifierPrefix + tokens.size();
					edgeC.getNextActive().getFeature("pos").unify(new StringRef(posId));
					newEdges.add(edgeC);
				} catch (UnificationFailedException ex) {}
			} else if (n.equals(">") || n.equals(">>") || n.equals("//")) {
				Edge edgeC = edge.deepCopy();
				edgeC.getNextActive().setFeature("*pos", tokens.size() + "");
				edgeC.addAntecedents(edgeC.getNextActive());
				newEdges.add(edgeC);
			} else if (n.equals("<")) {
				BackrefCategory bwrefCat = (BackrefCategory) edge.getNextActive();
				Category[] ante = edge.getCombinedAnteList();
				for (int i = ante.length-1 ; i >= 0 ; i--) {
					if (ante[i].getName().equals("//")) continue;
					
					boolean negMatch = false;
					for (FeatureMap negfm : bwrefCat.getNegFeatureMaps()) {
						if (ante[i].getFeatureMap().canUnify(negfm)) {
							negMatch = true;
							break;
						}
					}
					if (negMatch) continue;
					
					boolean posMatch = false;
					List<FeatureMap> posfms = bwrefCat.getPosFeatureMaps();
					for (int j = 0 ; j < posfms.size() ; j++) {
						if (ante[i].getFeatureMap().canUnify(posfms.get(j))) {
							try {
								Edge edgeC = edge.deepCopy();
								edgeC.getExternalAnteList()[i].getFeatureMap().unify(
										((BackrefCategory) edgeC.getNextActive()).getPosFeatureMaps().get(j)
									);
								backwardReferences.get(tokens.size()-1).add(
										edgeC.getExternalAnteList()[i].getFeatureMap().deepCopy()
									);
								newEdges.add(edgeC);
								posMatch = true;
							} catch (UnificationFailedException ex) {}
						}
					}
					if (posMatch) break;
				}
			} else if (n.equals("/<")) {
				Edge edgeC = edge.deepCopy();
				for (Category c : edge.getCombinedAnteList()) {
					if (c.getName().equals("//")) continue;
					if (c.getFeatureMap().canUnify(edge.getNextActive().getFeatureMap())) {
						edgeC = null;
						break;
					}
				}
				if (edgeC != null) {
					newEdges.add(edgeC);
				}
			} else {
				continue;
			}
			
			if (newEdges.isEmpty()) {
				if (debug) log.debug("CANNOT RESOLVE: {}", edge);
			}
			for (Edge newEdge : newEdges) {
				newEdge.step();
				boolean isNewEdge = chart.addEdge(newEdge);
				if (debug) log.debug("RESOLVE ({}): {}  --->  {}", (isNewEdge ? "NEW" : "KNOWN"), edge, newEdge);
			}
		}
		progressTable.put("resolution", l1.size());
	}
	
	private static List<Category> copyExceptionsList(List<Category> list) {
		List<Category> listCopy = new ArrayList<Category>();
		for (Category x : list) {
			listCopy.add(x.deepCopy());
		}
		return listCopy;
	}
}
