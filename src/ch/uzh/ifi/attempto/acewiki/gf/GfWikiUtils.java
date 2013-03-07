package ch.uzh.ifi.attempto.acewiki.gf;

import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.GeneralTopic;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;

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

}