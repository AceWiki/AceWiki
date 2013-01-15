package ch.uzh.ifi.attempto.acewiki.gfservice;

import nextapp.echo.app.event.ActionEvent;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.gui.ArticlePage;

public class GeneralPage extends ArticlePage {

	private static final long serialVersionUID = -7034483028750537141L;

	private final OntologyElement mElement;

	public GeneralPage(OntologyElement element, Wiki wiki) {
		super(wiki, element);
		mElement = element;
	}

	public OntologyElement getOntologyElement() {
		return mElement;
	}

	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
	}

	protected void doUpdate() {
		super.doUpdate();
		getTitle().setText(getHeading(mElement));
	}

}