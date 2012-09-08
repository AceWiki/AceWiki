package ch.uzh.ifi.attempto.acewiki.gfservice;

import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionEvent;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.gui.ArticlePage;

public class GeneralPage extends ArticlePage {

	private static final long serialVersionUID = -7034483028750537141L;

	// TODO: implement
	private static final String ACTION_REPARSE = "Reparse";

	private final OntologyElement mElement;

	public GeneralPage(OntologyElement element, Wiki wiki) {
		super(wiki, element);
		mElement = element;
		addTab(ACTION_REPARSE, this);
	}

	public OntologyElement getOntologyElement() {
		return mElement;
	}

	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		if (ACTION_REPARSE.equals(e.getActionCommand())) {
			log("page", "pressed: reparse");
			getWiki().showWindow(new WindowPane());
		}
	}

	protected void doUpdate() {
		super.doUpdate();
		getTitle().setText(getHeading(mElement));
	}

}