package ch.uzh.ifi.attempto.acewiki.gf;

import ch.uzh.ifi.attempto.acewiki.core.AceWikiGrammarEditor;
import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.GeneralTopic;
import ch.uzh.ifi.attempto.acewiki.core.GrammarEditorResult;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.gfservice.GfStorageResult;

public class GfGrammarEditor implements AceWikiGrammarEditor {

	private final GfGrammar mGfGrammar;

	public GfGrammarEditor(GfGrammar gfGrammar) {
		mGfGrammar = gfGrammar;
	}

	@Override
	public GrammarEditorResult update(Ontology ontology) {
		final GfStorageResult result;
		try {
			result = mGfGrammar.update();
			if (result.isSuccess()) {
				clearAllLinearizations(ontology);
			}
		} catch (GfServiceException e) {
			throw new RuntimeException(e);
		}
		return new GrammarEditorResult() {
			@Override
			public boolean isSuccess() {
				return result.isSuccess();
			}

			@Override
			public String getResultCode() {
				return result.getResultCode();
			}

			@Override
			public String getCommand() {
				return result.getCommand();
			}

			@Override
			public String getMessage() {
				return result.getMessage();
			}
		};
	}

	@Override
	public boolean isEditable() {
		return mGfGrammar.isGrammarEditable();
	}


	/**
	 * Clears all the multilingual linearizations of all the sentences
	 * in the whole wiki. This is called after grammar update to make sure
	 * that the sentences reflect the actual grammar.
	 *
	 * TODO: this should be replaced by something smarter, which only clears the
	 * linearizations of affected trees.
	 */
	private static void clearAllLinearizations(Ontology ontology) {
		for (GeneralTopic el : ontology.getOntologyElements(GeneralTopic.class)) {
			Article article = el.getArticle();
			for (Sentence sent : article.getSentences()) {
				if (sent instanceof GfSentence) {
					((GfSentence) sent).clearLinearizations();
				}
			}
		}
	}
}