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

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.Comment;
import ch.uzh.ifi.attempto.echocomp.EchoThread;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.TextArea;

/**
 * Customizable editor dialog for editing comments and grammar modules.
 * The provided builder can be used to set the required title, font,
 * size, etc. of the editor component and register a callback for the
 * "OK" button.
 *
 * @author Kaarel Kaljurand
 */
public class EditorDialog extends WindowPane implements ActionListener {

	private static final String ACTION_OK = "general_action_ok";
	private static final String ACTION_CANCEL = "general_action_cancel";

	private static final long serialVersionUID = 2167529316081897069L;
	private final TextArea mTextArea;
	private final Comment mComment;
	private final ArticlePage mPage;

	private Executable exPositiveButton;

	private EditorDialog(Comment comment, ArticlePage page) {
		mPage = page;
		mComment = comment;

		setModal(true);
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setResizable(false);
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");

		Column mainColumn = new Column();
		mainColumn.setInsets(new Insets(10, 10, 0, 0));
		mainColumn.setCellSpacing(new Extent(10));

		mTextArea = new TextArea();
		mTextArea.setText(mComment.getText());
		mainColumn.add(mTextArea);

		Row buttonBar = new Row();
		buttonBar.setAlignment(new Alignment(Alignment.RIGHT, Alignment.CENTER));
		buttonBar.setInsets(new Insets(0, 0, 10, 0));
		buttonBar.setCellSpacing(new Extent(5));
		GeneralButton okButton = new GeneralButton(ACTION_OK, this, 100);
		okButton.setActionCommand(ACTION_OK);
		buttonBar.add(okButton);
		GeneralButton cancelButton = new GeneralButton(ACTION_CANCEL, this, 100);
		cancelButton.setActionCommand(ACTION_CANCEL);
		buttonBar.add(cancelButton);
		mainColumn.add(buttonBar);

		add(mainColumn);

		setSize(600, 350);

		EchoThread.getActiveApplication().setFocusedComponent(mTextArea);
	}


	/**
	 * Sets the size of the text area window. The size of the text area is set accordingly.
	 *
	 * @param width The width of the window in pixels.
	 * @param height The height of the window in pixels.
	 */
	private void setSize(int width, int height) {
		if (width < 200) width = 200;
		if (height < 120) height = 120;
		setWidth(new Extent(width));
		setHeight(new Extent(height));
		mTextArea.setWidth(new Extent(width - 47));
		mTextArea.setHeight(new Extent(height - 108));
	}


	public void actionPerformed(ActionEvent e) {
		if (ACTION_OK.equals(e.getActionCommand())) {
			Article article = mPage.getArticle();
			Comment comment = article.getOntology().getStatementFactory()
					.createComment(mTextArea.getText(), article);
			article.edit(mComment, comment);
			mPage.getWiki().updateStatement(mComment, comment);
			mPage.update();
			mPage.getWiki().removeWindow(this);
			if (exPositiveButton != null) {
				exPositiveButton.execute();
			}
		} else if (ACTION_CANCEL.equals(e.getActionCommand())) {
			mPage.getWiki().removeWindow(this);
		}

		setVisible(false);
		dispose();
	}


	public static class Builder {
		private final EditorDialog mEditorDialog;
		public Builder(Comment comment, ArticlePage page) {
			mEditorDialog = new EditorDialog(comment, page);
		}
		public Builder setTitle(String title) {
			mEditorDialog.setTitle(title);
			return this;
		}
		public Builder setFont(Font font) {
			mEditorDialog.mTextArea.setFont(font);
			return this;
		}
		public Builder setSize(int width, int height) {
			mEditorDialog.setSize(width, height);
			return this;
		}
		public Builder setPositiveButton(Executable ex) {
			mEditorDialog.exPositiveButton = ex;
			return this;
		}
		public EditorDialog create() {
			return mEditorDialog;
		}
	}

}