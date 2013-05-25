package ch.uzh.ifi.attempto.acewiki.gf;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.StringDocumentSource;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLLogicalAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import com.google.common.base.Joiner;
import com.google.common.collect.HashMultiset;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Maps;
import com.google.common.collect.Multiset;
import com.google.common.collect.Sets;

import ch.uzh.ifi.attempto.acewiki.core.AceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyExporter;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.ACEText;
import ch.uzh.ifi.attempto.ape.OutputType;
import ch.uzh.ifi.attempto.gfservice.GfTree;
import ch.uzh.ifi.attempto.gfservice.GfTreeParseException;

/**
 * <p>Generates a report that covers all the articles and their sentences in the wiki,
 * along with the OWL mapping of the sentences. Reports various ambiguities:</p>
 *
 * <ul>
 * <li>number of trees per sentence</li>
 * <li>number of ACE sentences per sentence</li>
 * <li>number of OWL axioms per sentence</li>
 * </ul>
 *
 * <p>The format is optimized to be both easy to read and easy to process with grep+sed, e.g.
 * to get a list of all ambiguity types filter the output through:</p>
 *
 * <pre>
 * grep "___a" | sed "s/.*___t/t/" | sort | uniq -c | sort -nr
 *
 *   78 t1___a1___o1
 *   13 t1___a0___o0
 *    8 t1___a1___o0
 *    7 t2___a1___o1
 *    3 t4___a2___o2
 *    ...
 * </pre>
 *
 * TODO: currently experimental and GF specific
 *
 * @author Kaarel Kaljurand
 */
public class GfReportExporter extends OntologyExporter {

	private static OWLOntologyManager ontologyManager = OWLManager.createOWLOntologyManager();

	private static final String TAG_GF_SENTENCE = "gf_sentence";
	private static final String MAX_INDENT = "\t\t\t\t\t";
	private static final Joiner JOINER = Joiner.on("___").useForNull("NULL");

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
				if (s instanceof GfSentence && engine instanceof GfEngine) {
					GfGrammar gfGrammar = ((GfEngine) engine).getGfGrammar();
					statistics.add(TAG_GF_SENTENCE);
					GfSentence gfSent = (GfSentence) s;

					List<String> trees = gfSent.getParseTrees();
					statistics.add(TAG_GF_SENTENCE + "_tree_size_" + trees.size());

					AceReport aceReport = new AceReport(gfGrammar, trees);
					statistics.add(TAG_GF_SENTENCE + "_ace_size_" + aceReport.getAceAmbiguity());
					statistics.add(TAG_GF_SENTENCE + "_owl_size_" + aceReport.getOwlAmbiguity());

					String lastEditLanguage = gfSent.getGfWikiEntry().getLanguage();
					statistics.add(TAG_GF_SENTENCE + "_language_" + lastEditLanguage);

					addWithIndent(sb, 1,
							JOINER.join(s.isIntegrated(),
									lastEditLanguage,
									gfSent.getGfWikiEntry().getText(),
									"t" + s.getNumberOfRepresentations(),
									"a" + aceReport.getAceAmbiguity(),
									"o" + aceReport.getOwlAmbiguity())
							);

					int totalTreeSize = 0;
					int totalOwlSize = 0;
					for (String tree : trees) {
						statistics.add(TAG_GF_SENTENCE + "_tree");
						addWithIndent(sb, 2, tree);
						int treeSize = aceReport.getTreeSize(tree);
						totalTreeSize += treeSize;
						int owlSize = aceReport.getOwlSize(tree);
						totalOwlSize += owlSize;
						addWithIndent(sb, 3, "tree_size_" + treeSize);
						addWithIndent(sb, 3, aceReport.getAce(tree));
						addWithIndent(sb, 3, aceReport.getOwlFssPp(tree));
						addWithIndent(sb, 3, "owl_size_" + owlSize);
						addWithIndent(sb, 3, aceReport.getMessages(tree));
					}
					if (! trees.isEmpty()) {
						statistics.add(TAG_GF_SENTENCE + "_trees_treesize_" + (totalTreeSize / trees.size()));
						statistics.add(TAG_GF_SENTENCE + "_trees_owlsize_" + (totalOwlSize / trees.size()));
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


	private class AceReport {
		// Each tree corresponds to a set of 0 or more OWL axioms.
		// We store these sets into a single set. The number of elements in this set shows
		// the semantic ambiguity of the original sentence, which is smaller
		// or equal to the syntactic ambiguity (shown by the number of trees).
		// TODO: throw out axioms which have a semantically equivalent axiom in the set
		private Set<Set<OWLLogicalAxiom>> setOfSetofAxiom = Sets.newHashSet();
		private Map<String, ACEParserResult> treeToAceParserResult = Maps.newHashMap();
		// Linearization result
		private Map<String, String> treeToAce = Maps.newHashMap();
		// Linearization result that was correct ACE
		private Map<String, String> treeToAceParsed = Maps.newHashMap();
		// Pretty-printed OWL
		private Map<String, String> treeToOwl = Maps.newHashMap();

		private Map<String, Integer> treeToOwlSize = Maps.newHashMap();

		public AceReport(GfGrammar gfGrammar, List<String> trees) {
			for (String tree : trees) {
				if (tree == null) {
					continue;
				}

				try {
					ACEText acetext = GfWikiUtils.getACEText(gfGrammar, tree);
					if (acetext == null) continue;

					String acetextAsString = acetext.getText().trim();
					if (acetextAsString.isEmpty()) continue;

					treeToAce.put(tree, acetextAsString);
					ACEParserResult parserResult = GfWikiUtils.parse(acetext, getOntology().getURI());
					treeToAceParserResult.put(tree, parserResult);

					String drsAsString = parserResult.get(OutputType.DRS);
					if ("drs([],[])".equals(drsAsString)) continue;

					treeToAceParsed.put(tree, acetextAsString);
					String owlAsString = parserResult.get(OutputType.OWLFSSPP);
					Set<OWLLogicalAxiom> axiomSet = getLogicalAxiomsFromString(owlAsString);
					if (axiomSet.isEmpty()) continue;

					setOfSetofAxiom.add(axiomSet);
					treeToOwl.put(tree, owlAsString);

					// Number of different entities in the axiom set
					Set<OWLEntity> entities = Sets.newHashSet();
					for (OWLLogicalAxiom ax : axiomSet) {
						entities.addAll(ax.getSignature());
					}
					treeToOwlSize.put(tree, entities.size());
				} catch (Exception e) {
					continue;
				}
			}
		}

		public int getOwlAmbiguity() {
			return setOfSetofAxiom.size();
		}

		public int getAceAmbiguity() {
			return ImmutableSet.copyOf(treeToAceParsed.values()).size();
		}

		public String getAce(String tree) {
			return treeToAce.get(tree);
		}

		public int getTreeSize(String tree) {
			try {
				GfTree gfTree = new GfTree(tree);
				return gfTree.size();
			} catch (GfTreeParseException e) {
				return 0;
			}
		}

		public String getOwlFssPp(String tree) {
			return treeToOwl.get(tree);
		}

		public int getOwlSize(String tree) {
			Integer size = treeToOwlSize.get(tree);
			if (size == null) {
				return 0;
			}
			return size;
		}

		public String getMessages(String tree) {
			ACEParserResult aceParserResult = treeToAceParserResult.get(tree);
			if (aceParserResult == null) {
				return null;
			}
			return Joiner.on('\n').join(aceParserResult.getMessageContainer().getMessages());
		}
	}


	/**
	 * <p>Interprets the given string as a serialization of an OWL ontology,
	 * maps it to a set of OWL logical axioms and returns those.</p>
	 *
	 * @param str serialization of an OWL ontology e.g. in the OWLFSSPP format
	 * @return set of OWL logical axioms
	 * @throws OWLOntologyCreationException
	 *
	 * TODO: not sure if this is the most efficient way to do this
	 */
	public static Set<OWLLogicalAxiom> getLogicalAxiomsFromString(String str) throws OWLOntologyCreationException {
		OWLOntology owlOntology = ontologyManager.loadOntologyFromOntologyDocument(new StringDocumentSource(str));
		ontologyManager.removeOntology(owlOntology);
		return owlOntology.getLogicalAxioms();
	}
}