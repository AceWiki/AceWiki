// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.Comment;
import ch.uzh.ifi.attempto.acewiki.core.Statement;
import ch.uzh.ifi.attempto.echocomp.TextAreaWindow;

/**
 * This class manages the comment editor. It creates the editor window and handles its
 * responses.
 * 
 * @author Tobias Kuhn
 */
public class CommentEditorHandler implements ActionListener {
	
	private static final long serialVersionUID = 1156092885844135235L;
	
	private TextAreaWindow textAreaWindow;
	private Statement statement;
	private ArticlePage page;
	private boolean edit;
	
	/**
	 * Creates a new comment editor handler, either to create a new comment or to edit an existing
	 * comment.
	 * 
	 * @param statement The statement in front of which the new comment should be added (in the
	 *   case of edit=false) or the comment that should be edited (in the case of edit=true).
	 * @param page The host page of the comment.
	 * @param edit true if an existing comment should be edited, or false if a new comment should
	 *   be created.
	 */
	private CommentEditorHandler(Statement statement, ArticlePage page, boolean edit) {
		this.statement = statement;
		this.page = page;
		this.edit = edit;
		
		textAreaWindow = new TextAreaWindow(Wiki.getGUIText("acewiki_commeditor_title"), this);
		textAreaWindow.setSize(600, 350);
		if (edit) {
			textAreaWindow.setText(((Comment) statement).getText());
		}
	}

	/**
	 * Generates a new comment editor window for the creation of a new comment.
	 * 
	 * @param followingStatement The statement in front of which the new sentences should be added,
	 *   or null if the sentences should be added to the end of the article.
	 * @param page The host page into which the sentence should be added.
	 * @return A new preditor window.
	 */
	public static TextAreaWindow generateCreationWindow(Statement followingStatement,
			ArticlePage page) {
		CommentEditorHandler h = new CommentEditorHandler(followingStatement, page, false);
		return h.getWindow();
	}

	/**
	 * Generates a new comment editor window for editing an existing comment.
	 * 
	 * @param comment The comment that should be edited.
	 * @param page The host page which contains the sentence to be edited.
	 * @return A new preditor window.
	 */
	public static TextAreaWindow generateEditWindow(Comment comment, ArticlePage page) {
		CommentEditorHandler h = new CommentEditorHandler(comment, page, true);
		return h.getWindow();
	}
	
	private TextAreaWindow getWindow() {
		return textAreaWindow;
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("OK")) {
			Article article = page.getArticle();
			Comment comment = article.getOntology().getStatementFactory()
				.createComment(textAreaWindow.getText(), article);
			if (edit) {
				article.edit(statement, comment);
				page.getWiki().updateStatement(statement, comment);
			} else {
				article.add(statement, comment);
			}
			page.update();
			page.getWiki().removeWindow(textAreaWindow);
		} else if (e.getActionCommand().equals("Cancel")) {
			page.getWiki().removeWindow(textAreaWindow);
		}
	}

}
