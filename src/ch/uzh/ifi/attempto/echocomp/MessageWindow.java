// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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
import nextapp.echo.app.ResourceImageReference;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.event.WindowPaneEvent;
import nextapp.echo.app.event.WindowPaneListener;
import nextapp.echo.app.layout.GridLayoutData;

/**
 * This is a convenience class for easy creation of message windows.
 * 
 * @author Tobias Kuhn
 */
public class MessageWindow extends WindowPane implements ActionListener {
	
	private static final long serialVersionUID = -6999194368016297503L;
	
	private ActionListener actionListener;

	/**
	 * Creates a new message window.
	 * 
	 * @param title The title of the window.
	 * @param image The image to be displayed above the text.
	 * @param message The message text.
	 * @param parent The parent window.
	 * @param actionListener The action-listener.
	 * @param options A list of options each represented by a button in the message window.
	 */
	public MessageWindow(String title, ResourceImageReference image, String message, WindowPane parent, ActionListener actionListener, String... options) {
		this.actionListener = actionListener;
		setTitle(title);
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setModal(true);
		setWidth(new Extent(420));
		setHeight(new Extent(180));
		setResizable(false);
		setMovable(true);
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");
		
		addWindowPaneListener(new WindowPaneListener() {
			
			private static final long serialVersionUID = -3897741327122083261L;

			public void windowPaneClosing(WindowPaneEvent e) {
				actionPerformed(new ActionEvent(MessageWindow.this, "Close"));
			}
			
		});
		
		Grid grid = new Grid(1);
		grid.setInsets(new Insets(10, 10, 10, 0));
		grid.setColumnWidth(0, new Extent(400));
		grid.setRowHeight(0, new Extent(90));
		
		Column iconMessageColumn = new Column();
		GridLayoutData layout1 = new GridLayoutData();
		layout1.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		iconMessageColumn.setLayoutData(layout1);
		
		if (image != null) {
			iconMessageColumn.add(new Label(image));
			iconMessageColumn.add(new VSpace(5));
		}
		
		Label label = new Label(message);
		label.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		iconMessageColumn.add(label);
		grid.add(iconMessageColumn);
		
		Row buttonBar = new Row();
		buttonBar.setCellSpacing(new Extent(10));
		buttonBar.setInsets(new Insets(0, 0, 0, 10));
		for (String s : options) {
			buttonBar.add(new GeneralButton(s, 80, this));
		}
		GridLayoutData layout2 = new GridLayoutData();
		layout2.setAlignment(new Alignment(Alignment.CENTER, Alignment.BOTTOM));
		buttonBar.setLayoutData(layout2);
		grid.add(buttonBar);
		
		add(grid);
		
		if (parent != null && parent.getPositionX() != null) {
			setPositionX(new Extent(parent.getPositionX().getValue() + (parent.getWidth().getValue() - getWidth().getValue())/2));
			setPositionY(new Extent(parent.getPositionY().getValue() + (parent.getHeight().getValue() - getHeight().getValue())/2));
		}
	}
	
	/**
	 * Creates a new message window.
	 * 
	 * @param title The title of the window.
	 * @param message The message text.
	 * @param parent The parent window.
	 * @param actionListener The action-listener.
	 * @param options A list of options each represented by a button in the message window.
	 */
	public MessageWindow(String title, String message, WindowPane parent, ActionListener actionListener, String... options) {
		this(title, null, message, parent, actionListener, options);
	}
	
	/**
	 * Creates a new message window.
	 * 
	 * @param title The title of the window.
	 * @param message The message text.
	 * @param parent The parent window.
	 * @param options A list of options each represented by a button in the message window.
	 */
	public MessageWindow(String title, String message, WindowPane parent, String... options) {
		this(title, null, message, parent, null, options);
	}
	
	/**
	 * Creates a new message window.
	 * 
	 * @param title The title of the window.
	 * @param message The message text.
	 * @param options A list of options each represented by a button in the message window.
	 */
	public MessageWindow(String title, String message, String... options) {
		this(title, null, message, null, null, options);
	}
	
	public void actionPerformed(ActionEvent e) {
		setVisible(false);
		if (actionListener != null) {
			actionListener.actionPerformed(new ActionEvent(this, e.getActionCommand()));
		}
	}

}
