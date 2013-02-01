package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.List;

import nextapp.echo.app.Column;
import nextapp.echo.app.Font;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiEngine;
import ch.uzh.ifi.attempto.acewiki.core.Comment;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Statement;
import ch.uzh.ifi.attempto.acewiki.gui.ArticlePage;
import ch.uzh.ifi.attempto.acewiki.gui.CommentEditorHandler;
import ch.uzh.ifi.attempto.acewiki.gui.StatementMenu;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.gfservice.GfModule;
import ch.uzh.ifi.attempto.gfservice.GfParseResult;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.gfservice.GfStorageResult;

/**
 * TODO: localize strings
 *
 * @author Kaarel Kaljurand
 */
public class GfModulePage extends ArticlePage {

	private final Logger mLogger = LoggerFactory.getLogger(GfModulePage.class);

	private static final long serialVersionUID = -5592272938081004472L;

	// TODO: make it a button (not a tab), and enable it only if the grammar is error free
	private static final String ACTION_MAKE = "Rebuild grammar";

	private static final String acewiki_statementmenu_addgfmodule = "Add GF module content";

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
		getTitle().setColor(Style.specialForeground);
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
		} else if (acewiki_statementmenu_addgfmodule.equals(e.getActionCommand())) {
			if (!getWiki().isEditable()) {
				getWiki().showLoginWindow();
			} else {
				getWiki().showWindow(CommentEditorHandler.generateCreationWindow(null, this));
			}
		}
	}


	protected void doUpdate() {
		Column textColumn = getTextColumn();
		textColumn.removeAll();

		// TODO: hack: Treat the first comment in the GF Module Page as a GF module
		Comment grammarContent = getGrammarContent();
		if (grammarContent == null) {
			textColumn.add(new SolidLabel("(grammar module is empty)", Font.ITALIC, 10));
			if (! getWiki().isReadOnly()) {
				StatementMenu dropDown = getDropDown();
				dropDown = new StatementMenu(StatementMenu.EMPTY_TYPE, getWiki(), this);
				dropDown.addMenuEntry(acewiki_statementmenu_addgfmodule, "acewiki_statementmenu_addgfmoduletooltip");
				Row addButtonRow = new Row();
				addButtonRow.add(dropDown);
				textColumn.add(addButtonRow);
			}
		} else {
			textColumn.add(new GfModuleComponent(grammarContent, this));
		}

		getTitle().setText(mElement.getWord());
		parse(); // TODO: we should not parse every time the page is visited
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
		return new GfModule(getName(), getGrammarContent().getText());
	}


	private boolean hasContent() {
		return getGrammarContent() != null;
	}


	private String getName() {
		return mElement.getWord();
	}


	private Comment getGrammarContent() {
		List<Statement> statements = getArticle().getStatements();
		if (statements == null || statements.isEmpty() || ! (statements.get(0) instanceof Comment)) {
			return null;
		}
		return (Comment) statements.get(0);
	}
}