package ch.uzh.ifi.attempto.acewiki.gui;

import nextapp.echo.app.Color;
import nextapp.echo.app.Component;
import nextapp.echo.app.Font;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;

// TODO: merge this class with some other class (see e.g. echocomp)
public class GuiUtils {

	public static final Color COLOR_GF_MODULE = new Color(102, 153, 0);

	/**
	 * Returns a link or a label, depending on if the given
	 * name is in the ontology.
	 */
	public static Component getNameComponent(Wiki wiki, String name) {
		OntologyElement ol = wiki.getOntology().getElement(name);
		if (ol == null) {
			return new SolidLabel(name, Font.BOLD, 12);
		}
		return new WikiLink(ol, name, wiki, false);
	}

}