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

import java.util.Collection;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.ApplicationInstance;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.Insets;
import nextapp.echo.app.ResourceImageReference;
import nextapp.echo.app.Row;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.event.WindowPaneEvent;
import nextapp.echo.app.event.WindowPaneListener;
import nextapp.echo.app.layout.GridLayoutData;

/**
 * This is a convenience class for the creation of multiple choice windows.
 * 
 * @author Kaarel Kaljurand
 */
public class MultipleChoiceWindow extends GeneralWindow implements ActionListener {

	private static final long serialVersionUID = 607616571978106335L;
	private ActionListener mActionListener;


	public MultipleChoiceWindow(String title, ResourceImageReference image, String message,
			Collection<String>  choices,
			WindowPane parent, ActionListener actionListener) {
		mActionListener = actionListener;
		setTitle(title);
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setModal(true);
		setWidth(new Extent(420));
		setHeight(new Extent(100 * choices.size()));
		setResizable(false);
		setMovable(true);
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");

		addWindowPaneListener(new WindowPaneListener() {

			private static final long serialVersionUID = -1107201830529731000L;

			public void windowPaneClosing(WindowPaneEvent e) {
				actionPerformed(new ActionEvent(MultipleChoiceWindow.this, "Close"));
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

		// Choices
		Column choicesColumn = new Column();
		for (String choice : choices) {
			CheckBox cb = new CheckBox(choice);
			choicesColumn.add(cb);
		}
		grid.add(choicesColumn);

		// Buttons
		Row buttonBar = new Row();
		buttonBar.setCellSpacing(new Extent(10));
		buttonBar.setInsets(new Insets(0, 0, 0, 10));
		buttonBar.add(new GeneralButton("OK", this, 80));
		buttonBar.add(new GeneralButton("Cancel", this, 80));
		ApplicationInstance.getActive().setFocusedComponent(buttonBar.getComponent(0));

		GridLayoutData layout2 = new GridLayoutData();
		layout2.setAlignment(new Alignment(Alignment.CENTER, Alignment.BOTTOM));
		buttonBar.setLayoutData(layout2);
		grid.add(buttonBar);

		add(grid);

		//setCentered(parent);
	}


	public MultipleChoiceWindow(String title, String message, Collection<String> choises, WindowPane parent,
			ActionListener actionListener) {
		this(title, null, message, choises, parent, actionListener);
	}


	public MultipleChoiceWindow(String title, String message, Collection<String> choises, WindowPane parent) {
		this(title, null, message, choises, parent, null);
	}


	public MultipleChoiceWindow(String title, String message, Collection<String> choises) {
		this(title, null, message, choises, null, null);
	}


	public void actionPerformed(ActionEvent e) {
		setVisible(false);
		if (mActionListener != null) {
			mActionListener.actionPerformed(new ActionEvent(this, e.getActionCommand()));
		}
	}

}
