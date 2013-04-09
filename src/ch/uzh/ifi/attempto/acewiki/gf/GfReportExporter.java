package ch.uzh.ifi.attempto.acewiki.gf;

import static ch.uzh.ifi.attempto.ape.OutputType.OWLFSS;
import static ch.uzh.ifi.attempto.ape.OutputType.OWLFSSPP;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

import com.google.common.base.Joiner;
import com.google.common.base.Strings;
import com.google.common.collect.HashMultiset;
import com.google.common.collect.Maps;
import com.google.common.collect.Multiset;
import com.google.common.collect.Sets;

import ch.uzh.ifi.attempto.acewiki.core.AceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyExporter;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.ape.ACEParser;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.ACEText;
import ch.uzh.ifi.attempto.base.APE;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;

/**
 * <p>Generates a report that covers all the articles and their sentences in the wiki,
 * along with the OWL mapping of the sentences.</p>
 *
 * TODO: currently experimental and GF specific
 *
 * @author Kaarel Kaljurand
 */
public class GfReportExporter extends OntologyExporter {

	private static final String MAX_INDENT = "\t\t\t\t\t";
	private static final Joiner JOINER = Joiner.on("___");

	protected void writeContent(String language) throws IOException {
		StringBuilder sb = new StringBuilder();
		Multiset<String> statistics = HashMultiset.create();
		AceWikiEngine engine = getOntology().getEngine();

		for (OntologyElement oe : getOntologyElements()) {
			statistics.add("ontology_element");
			sb.append(oe.getWord());
			addWithIndent(sb, 0,
					JOINER.join(oe, oe.getArticle().getSentences().size()));
			for (Sentence s : oe.getArticle().getSentences()) {
				statistics.add("sentence");
				if (s instanceof GfDeclaration && engine instanceof GfEngine) {
					GfGrammar gfGrammar = ((GfEngine) engine).getGfGrammar();
					statistics.add("gf_declaration");
					GfDeclaration gfDecl = (GfDeclaration) s;

					List<String> trees = gfDecl.getParseTrees();
					statistics.add("gf_declaration_tree_size_" + trees.size());

					AceReport aceReport = new AceReport(gfGrammar, trees);
					statistics.add("gf_declaration_owl_size_" + aceReport.getOwlAmbiguity());

					String lastEditLanguage = gfDecl.getGfWikiEntry().getLanguage();
					statistics.add("gf_declaration_language_" + lastEditLanguage);

					addWithIndent(sb, 1,
							JOINER.join(s.isIntegrated(),
									lastEditLanguage,
									gfDecl.getGfWikiEntry().getText(),
									s.getNumberOfRepresentations(),
									aceReport.getOwlAmbiguity())
							);

					for (String tree : trees) {
						statistics.add("gf_declaration_tree");
						addWithIndent(sb, 2, tree);
						addWithIndent(sb, 3, aceReport.getAce(tree));
						addWithIndent(sb, 3, aceReport.getOwlFssPp(tree));
						addWithIndent(sb, 3, aceReport.getMessages(tree));
					}
				} else {
					addWithIndent(sb, 1,
							JOINER.join(s.isIntegrated(), s.toString(), s.getNumberOfRepresentations()));					
				}
			}
		}
		// Show some overall statistics, e.g. how many sentences have 0, 1, 1+ corresponding OWL representations
		SortedSet<String> sortedSet = Sets.newTreeSet(statistics.elementSet());
		for (String key : sortedSet) {
			addWithIndent(sb, 0, JOINER.join(key, statistics.count(key)));
		}
		write(sb.toString());
	}

	public String getName() {
		return "GF Report";
	}

	public boolean isApplicable() {
		return true;
	}

	public String getFileSuffix() {
		return ".gfreport.txt";
	}

	public String getContentType() {
		return "text/plain";
	}

	private void addWithIndent(StringBuilder sb, int level, String str) {
		String indent = MAX_INDENT.substring(0, level);
		sb.append(indent);
		if (str == null) {
			sb.append("NULL");
		} else {
			// Add indent after every newline
			sb.append(str.replaceAll("\\n", "\n" + indent));
		}
		sb.append('\n');
	}


	/**
	 * This was taken from:
	 * ch.uzh.ifi.attempto.acewiki.aceowl.ACESentence
	 */
	private static ACEParserResult parse(ACEText acetext, String uri) {
		ACEParser ape = APE.getParser();
		synchronized (ape) {
			ape.setURI(uri);
			ape.setGuessingEnabled(false);
			ape.setClexEnabled(false);

			return ape.getMultiOutput(
					acetext.getText(),
					acetext.getLexicon(),
					OWLFSS,
					OWLFSSPP
					);
		}
	}


	private class AceReport {
		private Set<String> owls = Sets.newHashSet(); // Syntactic equivalence check, TODO: make it semantic
		private Map<String, ACEParserResult> treeToAceParserResult = Maps.newHashMap();
		private Map<String, ACEText> treeToAce = Maps.newHashMap();

		public AceReport(GfGrammar gfGrammar, List<String> trees) {
			for (String tree : trees) {
				if (tree == null) {
					continue;
				}

				Set<String> lins = null;
				String targetLang = gfGrammar.getGrammar().getName() + GfGrammar.SUFFIX_APE;
				try {
					lins = gfGrammar.linearize(tree, targetLang);
				} catch (GfServiceException e) {
					// "ERROR in translation to " + targetLang + ": "+ e.getMessage()
					continue;
				}

				if (lins == null || lins.size() != 1) {
					// "ERROR: Bad linearizations"
					return;
				}

				ACEText acetext = new ACEText(lins.iterator().next());
				ACEParserResult parserResult = parse(acetext, getOntology().getURI());
				treeToAce.put(tree, acetext);
				treeToAceParserResult.put(tree, parserResult);

				// TODO: add only if OWL is a non-empty ontology
				String owlfss = parserResult.get(OWLFSS);
				if (owlfss != null && ! owlfss.isEmpty()) {
					owls.add(owlfss);
				}
			}
		}

		public int getOwlAmbiguity() {
			return owls.size();
		}

		public String getAce(String tree) {
			ACEText acetext = treeToAce.get(tree);
			if (acetext == null) {
				return null;
			}
			return acetext.getText();
		}

		public String getOwlFssPp(String tree) {
			ACEParserResult aceParserResult = treeToAceParserResult.get(tree);
			if (aceParserResult == null) {
				return null;
			}
			return Strings.emptyToNull(aceParserResult.get(OWLFSSPP));
		}

		public String getMessages(String tree) {
			ACEParserResult aceParserResult = treeToAceParserResult.get(tree);
			if (aceParserResult == null) {
				return null;
			}
			return Joiner.on('\n').join(aceParserResult.getMessageContainer().getMessages());
		}
	}
}