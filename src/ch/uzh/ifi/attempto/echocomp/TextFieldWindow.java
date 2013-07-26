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

package ch.uzh.ifi.attempto.echocomp;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;

public class TextFieldWindow extends WindowPane implements ActionListener {
	
	private static final long serialVersionUID = 4671851648820077010L;

	private ActionListener actionListener;
	
	private TextField textField;
	
	public TextFieldWindow(String title, String label, ActionListener actionListener) {
		this.actionListener = actionListener;
		
		setModal(true);
		setTitle(title);
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setResizable(false);
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");

		Grid grid = new Grid(1);
		grid.setInsets(new Insets(10, 10, 0, 0));
		grid.setColumnWidth(0, new Extent(400));
		grid.setRowHeight(0, new Extent(90));
		
		Column mainColumn = new Column();
		mainColumn.setCellSpacing(new Extent(10));
		grid.add(mainColumn);
		
		mainColumn.add(new SolidLabel(label));
		textField = new TextField();
		textField.setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(13)));
		textField.setWidth(new Extent(370));
		mainColumn.add(textField);
		
		Row buttonBar = new Row();
		buttonBar.setAlignment(new Alignment(Alignment.CENTER, Alignment.CENTER));
		buttonBar.setInsets(new Insets(0, 0, 10, 0));
		buttonBar.setCellSpacing(new Extent(5));
		GeneralButton okButton = new GeneralButton("general_action_ok", this, 100);
		okButton.setActionCommand("OK");
		buttonBar.add(okButton);
		GeneralButton cancelButton = new GeneralButton("general_action_cancel", this, 100);
		cancelButton.setActionCommand("Cancel");
		buttonBar.add(cancelButton);
		grid.add(buttonBar);
		
		add(grid);

		setWidth(new Extent(420));
		setHeight(new Extent(180));

		EchoThread.getActiveApplication().setFocusedComponent(textField);
	}
	
	public String getText() {
		return textField.getText();
	}
	
	public void setText(String text) {
		textField.setText(text);
	}
	
	public void actionPerformed(ActionEvent e) {
		if (actionListener != null) {
			actionListener.actionPerformed(new ActionEvent(this, e.getActionCommand()));
		}
		setVisible(false);
		dispose();
	}

}
