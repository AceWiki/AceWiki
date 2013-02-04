package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.List;

import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Component;
import nextapp.echo.app.Extent;
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
import ch.uzh.ifi.attempto.acewiki.gui.EditorDialog;
import ch.uzh.ifi.attempto.acewiki.gui.Executable;
import ch.uzh.ifi.attempto.acewiki.gui.GrammarPage;
import ch.uzh.ifi.attempto.acewiki.gui.WikiLink;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.VSpace;
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

	private static final String ACTION_EDIT = "Edit module";
	private static final String ACTION_CHECK = "Check module";
	private static final String ACTION_MAKE = "Rebuild grammar";

	private final OntologyElement mElement;
	private final GFEngine mEngine;
	private final Wiki mWiki;

	private Row mFormattedModuleContent;

	public GfModulePage(OntologyElement element, Wiki wiki) {
		super(wiki, element);
		mElement = element;
		mWiki = wiki;

		AceWikiEngine engine = getArticle().getOntology().getEngine();
		if (engine instanceof GFEngine) {
			mEngine = (GFEngine) engine;
		} else {
			mEngine = null;
		}
		getTitle().setColor(Style.specialForeground);
	}

	public OntologyElement getOntologyElement() {
		return mElement;
	}


	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		if (ACTION_MAKE.equals(e.getActionCommand())) {
			if (!getWiki().isEditable()) {
				getWiki().showLoginWindow();
			} else {
				integrate();
			}
		} else if (ACTION_EDIT.equals(e.getActionCommand())) {
			if (!getWiki().isEditable()) {
				getWiki().showLoginWindow();
			} else {
				EditorDialog.Builder editor = new EditorDialog.Builder(getGrammarContent(), this)
				.setTitle("GF Module Editor")
				.setSize(600, 600)
				.setFont(new Font(Font.MONOSPACE, Font.PLAIN, new Extent(12)))
				.setPositiveButton(new Executable() {
					@Override
					public void execute() {
						parse(false);
					}
				});
				mWiki.showWindow(editor.create());
			}
		} else if (ACTION_CHECK.equals(e.getActionCommand())) {
			if (!getWiki().isEditable()) {
				getWiki().showLoginWindow();
			} else {
				parse(true);
			}
		}
	}


	protected void doUpdate() {
		Column textColumn = getTextColumn();
		textColumn.removeAll();

		Row buttonRow = new Row();
		buttonRow.add(new GeneralButton(ACTION_EDIT, this));
		buttonRow.add(new HSpace(8));
		buttonRow.add(new GeneralButton(ACTION_CHECK, this));
		buttonRow.add(new HSpace(8));
		buttonRow.add(new GeneralButton(ACTION_MAKE, this));
		buttonRow.add(new VSpace(40));
		textColumn.add(buttonRow);

		Comment grammarContent = getGrammarContent();
		if (grammarContent == null) {
			GrammarPage.replaceModuleContent(getArticle(), makeDefaulContent());
			grammarContent = getGrammarContent();
		}
		mFormattedModuleContent = getGfModuleColumn(grammarContent.getText());
		textColumn.add(mFormattedModuleContent);

		getTitle().setText(mElement.getWord());
	}


	private void integrate() {
		// TODO: this blocks, do it in the background
		if (mEngine.getGFGrammar().isGrammarEditable() && hasContent()) {
			try {
				GfStorageResult result = mEngine.getGFGrammar().integrateGfModule(getGfModule());

				if (result.isSuccess()) {
					mWiki.showWindow(new MessageWindow("OK", "Grammar rebuilt successfully"));
				} else {
					mWiki.showWindow(new MessageWindow(result.getResultCode(),
							result.getMessage() + " (" + result.getCommand() + ")"));
				}
			} catch (GfServiceException e) {
				mLogger.info("make: GfServiceException: '{}'", e.getMessage());
			}
		}
	}


	private void parse(boolean popupOnSuccess) {
		// TODO: this blocks, do it in the background
		if (hasContent()) {
			try {
				GfParseResult result = mEngine.getGFGrammar().parseGfModule(getGfModule());
				if (result.isSuccess()) {
					if (popupOnSuccess) {
						mWiki.showWindow(new MessageWindow("OK", "There are no syntax errors."));
					}
				} else {
					// Pop up error message
					mLogger.info("parse: GfParseResult: '{}'", result);
					mWiki.showWindow(new MessageWindow(
							"Syntax error at line:column = " + result.getLocation(),
							result.getResultCode()));
					String line = result.getLocation().split(":")[0];
					highlightSyntaxError(Integer.parseInt(line));
				}
			} catch (GfServiceException e) {
				mLogger.info("parse: GfServiceException: '{}'", e.getMessage());
			}
		}
	}


	private void highlightSyntaxError(int nth1) {
		if (mFormattedModuleContent != null &&
				mFormattedModuleContent.getComponent(0) != null &&
				nth1 <= mFormattedModuleContent.getComponent(0).getComponentCount()) {
			mFormattedModuleContent.getComponent(0).getComponent(nth1 - 1).setBackground(Color.PINK);
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


	private String makeDefaulContent() {
		return "resource " + mElement.getWord() + " = {\n\n\t\n}";
	}


	private Comment getGrammarContent() {
		List<Statement> statements = getArticle().getStatements();
		if (statements == null || statements.isEmpty() || ! (statements.get(0) instanceof Comment)) {
			return null;
		}
		return (Comment) statements.get(0);
	}


	private Row getGfModuleColumn(String text) {
		Column colNumbers = new Column();
		Column colLines = new Column();
		int lineNumber = 0;
		for (String s : text.split("\\n")) {
			lineNumber++;
			Row rowNumber = new Row();
			rowNumber.add(makeLineNumber(lineNumber));
			colNumbers.add(rowNumber);

			Row rowLine = new Row();
			rowLine.add(markupLine(s));
			colLines.add(rowLine);
		}
		Row row = new Row();
		row.add(colNumbers);
		row.add(new HSpace(20));
		row.add(colLines);
		return row;
	}


	private Component markupLine(String text) {
		StringBuilder sb = new StringBuilder();
		Row row = new Row();
		for (String s : modifyText(text).split("~b")) {
			// Check if the element is in the ontology
			OntologyElement oe = mWiki.getOntology().getElement(s);
			if (oe == null) {
				sb.append(s);
			} else {
				if (sb.length() > 0) {
					row.add(makePlainText(sb.toString()));
					sb.setLength(0);
				}
				row.add(new HSpace());
				row.add(new WikiLink(oe, s, mWiki, false));
				row.add(new HSpace());
			}
		}
		row.add(makePlainText(sb.toString()));
		return row;
	}

	private String modifyText(String text) {
		text = text.replaceAll("([a-zA-Z0-9_-]+)", "~b$1~b");
		return text;
	}

	/**
	 * Turns the given string into a label.
	 * Empty strings also get a representation, otherwise they would not create a row.
	 */
	private static Component makePlainText(String s) {
		if (s.isEmpty()) {
			s = "\u00A0";
		}
		SolidLabel label = new SolidLabel(s, Font.PLAIN);
		label.setForeground(Style.darkForeground);
		label.setFont(new Font(Font.MONOSPACE, Font.PLAIN, new Extent(12)));
		label.setFormatWhitespace(true); // TODO: maybe this should be part of any SolidLabel
		return label;
	}


	private static SolidLabel makeLineNumber(int lineNumber) {
		SolidLabel label = new SolidLabel(String.format("%4d", lineNumber), Font.PLAIN);
		label.setForeground(new Color(120, 120, 120));
		label.setFont(new Font(Font.MONOSPACE, Font.PLAIN, new Extent(12)));
		label.setFormatWhitespace(true); // TODO: maybe this should be part of any SolidLabel
		return label;
	}
}