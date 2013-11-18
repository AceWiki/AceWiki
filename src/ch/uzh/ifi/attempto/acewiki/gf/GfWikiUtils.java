package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.Set;

import ch.uzh.ifi.attempto.ape.ACEParser;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.ACEText;
import ch.uzh.ifi.attempto.ape.OutputType;
import ch.uzh.ifi.attempto.base.APE;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;

public class GfWikiUtils {

	public static ACEParserResult parse(ACEText acetext, String uri, OutputType... outputTypes) {
		ACEParser ape = APE.getParser();
		synchronized (ape) {
			ape.setURI(uri);
			ape.setClexEnabled(false);

			return ape.getMultiOutput(
					acetext.getText(),
					acetext.getLexicon(),
					outputTypes
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