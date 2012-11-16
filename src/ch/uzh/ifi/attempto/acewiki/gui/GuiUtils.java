package ch.uzh.ifi.attempto.acewiki.gui;

import nextapp.echo.app.Component;
import nextapp.echo.app.Font;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;

public class GuiUtils {

	/**
	 * Returns a link or a label, depending on if the given
	 * name is in the ontology.
	 */
	public static Component getNameComponent(Wiki wiki, String name) {
		OntologyElement ol = wiki.getOntology().getElement(name);
		if (ol == null) {
			return new SolidLabel(name, Font.BOLD, 12);
		}
		// TODO: we currently use red (i.e. true) just to highlight what is a link.
		// It would be better to use blue (currently blue appears only during roll-over).
		return new WikiLink(ol, name, wiki, true);
	}

}