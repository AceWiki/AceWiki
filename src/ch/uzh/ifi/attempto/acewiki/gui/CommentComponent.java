// This file is part of AceWiki.
// Copyright 2008-2011, Tobias Kuhn.
// 
// AceWiki is free software: you can redistribute it and/or modify it under the terms of the GNU
// Lesser General Public License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
// 
// AceWiki is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License along with AceWiki. If
// not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.acewiki.gui;

import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

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
import ch.uzh.ifi.attempto.acewiki.gui.page.ArticlePage;
import ch.uzh.ifi.attempto.acewiki.gui.page.WikiPage;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a comment component consisting of a drop down menu and a comment text.
 * 
 * @author Tobias Kuhn
 */
public class CommentComponent extends Column implements ActionListener {

	private static final long serialVersionUID = -540135972060005725L;
	
	private static final int COMMENT_TEXT_WIDTH = 800;
	
	private static FontMetrics fontMetrics =
		((Graphics2D) new BufferedImage(2, 2, BufferedImage.TYPE_4BYTE_ABGR_PRE)
		.createGraphics()).getFontMetrics(new java.awt.Font("Verdana", java.awt.Font.ITALIC, 13));
	
	private Comment comment;
	private Wiki wiki;
	private WikiPage hostPage;
	
	private Row commentRow = new Row();
	private StatementMenu dropDown;
	
	/**
	 * Creates a new comment row.
	 * 
	 * @param comment The comment to be shown.
	 * @param hostPage The host page of the comment row.
	 */
	public CommentComponent(Comment comment, WikiPage hostPage) {
		this.comment = comment;
		this.hostPage = hostPage;
		this.wiki = hostPage.getWiki();
		update();
	}
	
	private void update() {
		dropDown = new StatementMenu(StatementMenu.COMMENT_TYPE, wiki, this);
		if (!wiki.isReadOnly()) {
			dropDown.addMenuEntry("Edit...", "Edit this comment");
			dropDown.addMenuEntry("Delete", "Delete this comment from the article");
			dropDown.addMenuSeparator();
			dropDown.addMenuEntry("Add Sentence...", "Add a new sentence here");
			dropDown.addMenuEntry("Add Comment...", "Add a new comment here");
		} else {
			// TODO: Check whether this is still needed
//			dropDown.setPopUp(null);
//			dropDown.setPopUpBorder(new Border(0, Color.BLACK, Border.STYLE_SOLID));
		}
		RowLayoutData layout = new RowLayoutData();
		layout.setAlignment(new Alignment(Alignment.CENTER, Alignment.TOP));
		dropDown.setLayoutData(layout);
		Column c = new Column();
		for (String s : (comment.getText() + " ").split("\\n")) {
			int indent = s.replaceFirst("^(\\s*).*$", "$1").length() * 5;
			s = s.replaceFirst("^\\s*", "");
			if (indent > COMMENT_TEXT_WIDTH/2) indent = COMMENT_TEXT_WIDTH/2;
			for (Component comp : wrapText(s, COMMENT_TEXT_WIDTH-indent)) {
				Row r = new Row();
				r.add(new VSpace(17));
				r.add(new HSpace(indent));
				r.add(comp);
				c.add(r);
			}
		}
		
		removeAll();
		commentRow.removeAll();
		commentRow.add(dropDown);
		commentRow.add(new HSpace(5));
		commentRow.add(c);
		commentRow.add(new HSpace(10));
		add(commentRow);
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("Edit...")) {
			wiki.log("page", "dropdown: edit comment: " + comment.getText());
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(CommentEditorHandler.generateEditWindow(
						comment,
						(ArticlePage) hostPage
					));
			}
		} else if (e.getActionCommand().equals("Add Sentence...")) {
			wiki.log("page", "dropdown: add sentence");
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(SentenceEditorHandler.generateCreationWindow(
						comment,
						(ArticlePage) hostPage
					));
			}
		} else if (e.getActionCommand().equals("Add Comment...")) {
			wiki.log("page", "dropdown: add comment");
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(CommentEditorHandler.generateCreationWindow(
						comment,
						(ArticlePage) hostPage
					));
			}
		} else if (e.getActionCommand().equals("Delete")) {
			wiki.log("page", "dropdown: delete sentence: " + comment.getText());
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
			} else {
				wiki.showWindow(new MessageWindow(
						"Delete",
						"Do you really want to delete this comment?",
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
	
	private List<Component> wrapText(String text, int width) {
		List<Component> wrappedText = new ArrayList<Component>();
		String line = "";
		Row row = new Row();
		text = text.replaceAll("~", "~t");
		while (text.matches(".*\\[\\[[^\\]]* [^\\]]*\\]\\].*")) {
			text = text.replaceAll("\\[\\[([^\\]]*) ([^\\]]*)\\]\\]", "[[$1_$2]]");
		}
		text = text.replaceAll(" ", " ~b");
		text = text.replaceAll("_of\\]\\]", " of]]");
		text = text.replaceAll("_by\\]\\]", " by]]");
		text = text.replaceAll("\\[\\[", "~b[[");
		text = text.replaceAll("\\]\\]", "]]~b");
		text = text.replaceAll("~t", "~");
		for (String s : text.split("~b")) {
			CommentPart cp = new CommentPart(s);
			if (line.length() == 0 || fontMetrics.stringWidth(line + cp.getText()) < width) {
				row.add(cp.getComponent());
				line += cp.getText();
				if (cp.getText().endsWith(" ")) row.add(new HSpace());
			} else {
				wrappedText.add(row);
				row = new Row();
				row.add(cp.getComponent());
				line = cp.getText();
				if (cp.getText().endsWith(" ")) row.add(new HSpace());
			}
		}
		if (line.length() > 0) {
			wrappedText.add(row);
		}
		return wrappedText;
	}
	
	
	private class CommentPart extends Component {
		
		private static final long serialVersionUID = 8522664422692717971L;
		
		private Component comp;
		private String text;
		
		public CommentPart(String s) {
			if (s.startsWith("http://") || s.startsWith("https://") || s.startsWith("ftp://")) {
				comp = new WebLink(s);
				text = s;
			} else if (s.startsWith("[[") && s.endsWith("]]")) {
				String name = s.substring(2, s.length()-2);
				OntologyElement oe = hostPage.getWiki().getOntology().getElement(name);
				if (oe != null) {
					int wn = oe.getIndexOfWord(name);
					comp = new WikiLink(oe, oe.getPrettyWord(wn), hostPage.getWiki(), false);
					text = name;
				}
			}
			if (comp == null) {
				SolidLabel label = new SolidLabel(s, Font.ITALIC);
				label.setForeground(new Color(120, 120, 120));
				comp = label;
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
