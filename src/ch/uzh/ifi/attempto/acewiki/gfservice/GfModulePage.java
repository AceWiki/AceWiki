package ch.uzh.ifi.attempto.acewiki.gfservice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import nextapp.echo.app.Column;
import nextapp.echo.app.Font;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.Comment;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Statement;
import ch.uzh.ifi.attempto.acewiki.gui.ArticlePage;
import ch.uzh.ifi.attempto.acewiki.gui.GfModuleComponent;
import ch.uzh.ifi.attempto.acewiki.gui.GuiUtils;
import ch.uzh.ifi.attempto.acewiki.gui.StatementMenu;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.gfservice.GfModule;
import ch.uzh.ifi.attempto.gfservice.GfParseResult;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.gfservice.GfStorageResult;

public class GfModulePage extends ArticlePage {

	private final Logger mLogger = LoggerFactory.getLogger(GfModulePage.class);

	private static final long serialVersionUID = -5592272938081004472L;

	// TODO: make it a button (not a tab), and enable it only if the grammar is error free
	private static final String ACTION_MAKE = "Rebuild grammar";

	private final OntologyElement mElement;
	private final GFEngine mEngine;

	public GfModulePage(OntologyElement element, Wiki wiki) {
		super(wiki, element);
		mElement = element;

		// TODO: improve
		AceWikiEngine engine = getArticle().getOntology().getEngine();
		if (engine instanceof GFEngine) {
			mEngine = (GFEngine) engine;
		} else {
			mEngine = null;
		}
		getTitle().setColor(GuiUtils.COLOR_GF_MODULE);
		addTab(ACTION_MAKE, this);
	}

	public OntologyElement getOntologyElement() {
		return mElement;
	}


	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		if (ACTION_MAKE.equals(e.getActionCommand())) {
			log("page", "pressed: make");
			integrate();
		}
	}

	protected void doUpdate() {
		Column textColumn = getTextColumn();
		StatementMenu dropDown = getDropDown();
		textColumn.removeAll();

		// Treat comments in the GF Module Page as GF Modules
		for (Statement s : getArticle().getStatements()) {
			if (s instanceof Comment) {
				textColumn.add(new GfModuleComponent((Comment) s, this));
			}
		}

		if (getArticle().getStatements().size() == 0) {
			textColumn.add(new SolidLabel("(grammar is empty)", Font.ITALIC, 10));
		}

		if (!getWiki().isReadOnly()) {
			Row addButtonRow = new Row();
			addButtonRow.add(dropDown);
			textColumn.add(addButtonRow);
		}
		getTitle().setText(mElement.getWord());
		parse();
	}


	private void integrate() {
		// TODO: this blocks, do it in the background
		if (mEngine.getGFGrammar().isGrammarEditable() && hasContent()) {
			try {
				GfStorageResult result = mEngine.getGFGrammar().integrateGfModule(getGfModule());

				if (! result.isSuccess()) {
					// Pop up error message
					mLogger.info("integrate: GfStorageResult: '{}'", result);
					getWiki().showWindow(new MessageWindow(result.getResultCode(),
							result.getMessage() + " (" + result.getCommand() + ")"));
				}
			} catch (GfServiceException e) {
				mLogger.info("make: GfServiceException: '{}'", e.getMessage());
			}
		}
	}


	private void parse() {
		// TODO: this blocks, do it in the background
		if (hasContent()) {
			try {
				GfParseResult result = mEngine.getGFGrammar().parseGfModule(getGfModule());
				if (! result.isSuccess()) {
					// Pop up error message
					mLogger.info("parse: GfParseResult: '{}'", result);
					getWiki().showWindow(new MessageWindow(
							"Syntax error at line:column = " + result.getLocation(),
							result.getResultCode()));
				}
			} catch (GfServiceException e) {
				mLogger.info("parse: GfServiceException: '{}'", e.getMessage());
			}
		}
	}


	private GfModule getGfModule() {
		return new GfModule(getName(), getContent());
	}


	private boolean hasContent() {
		return ! getArticle().getStatements().isEmpty();
	}


	private String getName() {
		return mElement.getWord();
	}


	private String getContent() {
		return getArticle().getStatements().iterator().next().toString();
	}

}