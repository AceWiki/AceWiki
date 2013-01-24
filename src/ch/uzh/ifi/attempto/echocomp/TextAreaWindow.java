// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

package ch.uzh.ifi.attempto.echocomp;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;

/**
 * This class represents a window that contains a text area plus "OK" and "Cancel" buttons.
 * 
 * @author Tobias Kuhn
 */
public class TextAreaWindow extends WindowPane implements ActionListener {
	
	private static final long serialVersionUID = -8910244587508187438L;
	
	private ActionListener actionListener;
	
	private TextArea textArea;
	
	/**
	 * Creates a new text area window with the given title, text, and action-listener.
	 * 
	 * @param title The title of the window.
	 * @param actionListener The action-listener or null.
	 */
	public TextAreaWindow(String title, ActionListener actionListener) {
		this.actionListener = actionListener;
		
		setModal(true);
		setTitle(title);
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setResizable(false);
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");
		
		Column mainColumn = new Column();
		mainColumn.setInsets(new Insets(10, 10, 0, 0));
		mainColumn.setCellSpacing(new Extent(10));
		
		textArea = new TextArea();
		textArea.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		mainColumn.add(textArea);
		
		Row buttonBar = new Row();
		buttonBar.setAlignment(new Alignment(Alignment.RIGHT, Alignment.CENTER));
		buttonBar.setInsets(new Insets(0, 0, 10, 0));
		buttonBar.setCellSpacing(new Extent(5));
		GeneralButton okButton = new GeneralButton("general_action_ok", this, 100);
		okButton.setActionCommand("OK");
		buttonBar.add(okButton);
		GeneralButton cancelButton = new GeneralButton("general_action_cancel", this, 100);
		cancelButton.setActionCommand("Cancel");
		buttonBar.add(cancelButton);
		mainColumn.add(buttonBar);
		
		add(mainColumn);
		
		setSize(450, 300);

		EchoThread.getActiveApplication().setFocusedComponent(textArea);
	}
	
	/**
	 * Returns the text of the text area.
	 * 
	 * @return The text of the text area.
	 */
	public String getText() {
		return textArea.getText();
	}
	
	/**
	 * Sets the text of the text area.
	 * 
	 * @param text The new text of the text area.
	 */
	public void setText(String text) {
		textArea.setText(text);
	}
	
	/**
	 * Sets the size of the text area window. The size of the text area is set accordingly.
	 * 
	 * @param width The width of the window in pixels.
	 * @param height The height of the window in pixels.
	 */
	public void setSize(int width, int height) {
		if (width < 200) width = 200;
		if (height < 120) height = 120;
		setWidth(new Extent(width));
		setHeight(new Extent(height));
		textArea.setWidth(new Extent(width - 47));
		textArea.setHeight(new Extent(height - 108));
	}
	
	public void actionPerformed(ActionEvent e) {
		if (actionListener != null) {
			actionListener.actionPerformed(new ActionEvent(this, e.getActionCommand()));
		}
		setVisible(false);
		dispose();
	}

}
