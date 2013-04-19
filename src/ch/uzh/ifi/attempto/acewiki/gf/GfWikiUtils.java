package ch.uzh.ifi.attempto.acewiki.gf;

import static ch.uzh.ifi.attempto.ape.OutputType.DRSPP;
import static ch.uzh.ifi.attempto.ape.OutputType.OWLFSS;
import static ch.uzh.ifi.attempto.ape.OutputType.OWLFSSPP;
import static ch.uzh.ifi.attempto.ape.OutputType.OWLXML;
import static ch.uzh.ifi.attempto.ape.OutputType.PARAPHRASE1;

import java.util.Set;

import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.GeneralTopic;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.ape.ACEParser;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.ACEText;
import ch.uzh.ifi.attempto.ape.OutputType;
import ch.uzh.ifi.attempto.base.APE;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;

public class GfWikiUtils {

	/**
	 * Clears all the multilingual linearizations of all the sentences
	 * in the whole wiki. This should be called after grammar update to make sure
	 * that the sentences reflect the actual grammar.
	 *
	 * TODO: this should be replaced by something smarter, which only clears the
	 * linearizations of affected trees.
	 */
	public static void clearAllLinearizations(Ontology ontology) {
		for (GeneralTopic el : ontology.getOntologyElements(GeneralTopic.class)) {
			Article article = el.getArticle();
			for (Sentence sent : article.getSentences()) {
				if (sent instanceof GfDeclaration) {
					((GfDeclaration) sent).clearLinearizations();
				}
			}
		}
	}


	public static ACEParserResult parse(ACEText acetext, String uri) {
		ACEParser ape = APE.getParser();
		synchronized (ape) {
			ape.setURI(uri);
			ape.setClexEnabled(false);

			return ape.getMultiOutput(
					acetext.getText(),
					acetext.getLexicon(),
					OutputType.DRS,
					PARAPHRASE1,
					OWLXML,
					OWLFSS,
					OWLFSSPP,
					DRSPP
					);
		}
	}


	/**
	 * Linearize the given tree in the Ape language and return the corresponding
	 * ACEText object.
	 */
	public static ACEText getACEText(GfGrammar gfGrammar, String tree) throws Exception {
		Set<String> lins = null;
		String targetLang = gfGrammar.getGrammar().getName() + GfGrammar.SUFFIX_APE;
		try {
			lins = gfGrammar.linearize(tree, targetLang);
		} catch (GfServiceException e) {
			throw new Exception("Failed to linearize into " + targetLang + ": " + e.getMessage());
		}

		if (lins == null || lins.size() != 1) {
			throw new Exception("Failed to linearize into " + targetLang + ": " + lins);
		}

		return new ACEText(lins.iterator().next().trim());
	}

}