package ch.uzh.ifi.attempto.acewiki.gui;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Component;
import nextapp.echo.app.Font;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.layout.RowLayoutData;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.Comment;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;

public class GfModuleComponent extends Column implements ActionListener {

	private static final long serialVersionUID = 6321385986210724477L;
	private final Logger mLogger = LoggerFactory.getLogger(GfModuleComponent.class);

	private Comment comment;
	private Wiki wiki;
	private WikiPage hostPage;

	private Row commentRow = new Row();
	private StatementMenu statementMenu;

	public GfModuleComponent(Comment comment, WikiPage hostPage) {
		this.comment = comment;
		this.hostPage = hostPage;
		this.wiki = hostPage.getWiki();
		update();
	}

	private void update() {
		statementMenu = new StatementMenu(StatementMenu.COMMENT_TYPE, wiki, this);
		if (!wiki.isReadOnly()) {
			statementMenu.addMenuEntry("Edit...", "Edit this module");
			statementMenu.addMenuEntry("Delete", "Delete this module");
		}
		RowLayoutData layout = new RowLayoutData();
		layout.setAlignment(new Alignment(Alignment.CENTER, Alignment.TOP));
		statementMenu.setLayoutData(layout);
		Column c = new Column();
		// For every line
		for (String s : (comment.getText() + " ").split("\\n")) {
			int indent = s.replaceFirst("^(\\s*).*$", "$1").length() * 5;
			s = s.replaceFirst("^\\s*", "");
			Row r = new Row();
			r.add(new VSpace(17));
			r.add(new HSpace(indent));
			r.add(markupText(s));
			c.add(r);
		}

		removeAll();
		commentRow.removeAll();
		commentRow.add(statementMenu);
		commentRow.add(new HSpace(5));
		commentRow.add(c);
		commentRow.add(new HSpace(10));
		add(commentRow);
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("Edit...")) {
			wiki.log("page", "dropdown: edit module: " + comment.getText());
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(CommentEditorHandler.generateEditWindow(
						comment,
						(ArticlePage) hostPage
						));
			}
		} else if (e.getActionCommand().equals("Delete")) {
			wiki.log("page", "dropdown: delete module: " + comment.getText());
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(new MessageWindow(
						"Delete",
						"Do you really want to delete this grammar module?",
						null,
						this,
						"Yes", "No"
						));
			}
		} else if (e.getSource() instanceof MessageWindow && e.getActionCommand().equals("Yes")) {
			wiki.log("page", "dropdown: delete confirmed: " + comment.getText());
			comment.getArticle().remove(comment);
			wiki.update();
			wiki.refresh();
		}
	}


	private Component markupText(String text) {
		Row row = new Row();
		for (String s : modifyText(text).split("~b")) {
			//mLogger.info("CommentPart: {}", s);
			CommentPart cp = new CommentPart(s);
			row.add(cp.getComponent());
			if (cp.getText().endsWith(" ")) row.add(new HSpace());
		}
		return row;
	}

	private String modifyText(String text) {
		text = text.replaceAll("([a-zA-Z0-9_-]+)", "~b$1~b");
		return text;
	}

	private class CommentPart extends Component {

		private static final long serialVersionUID = -8722454707702222523L;
		private Component comp;
		private String text;

		public CommentPart(String s) {
			// Check if the element is in the ontology
			OntologyElement oe = wiki.getOntology().getElement(s);
			if (oe == null) {
				SolidLabel label = new SolidLabel(s, Font.PLAIN);
				label.setForeground(new Color(120, 120, 120));
				comp = label;
				text = s;
			} else {
				comp = new WikiLink(oe, s, wiki, false);
				text = s;
			}
		}

		public Component getComponent() {
			return comp;
		}

		public String getText() {
			return text;
		}
	}
}