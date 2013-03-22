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

import echopoint.HtmlLabel;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.Insets;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.event.WindowPaneEvent;
import nextapp.echo.app.event.WindowPaneListener;

/**
 * This is a convenience class for easy creation of message windows,
 * where the message content is wrapped in "<pre>" and formatted in HTML.
 */
public class SimpleErrorMessageWindow extends WindowPane implements ActionListener {

	private static final long serialVersionUID = -2917888440906631987L;

	public SimpleErrorMessageWindow(String title, String message) {
		setTitle(localize(title));
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setModal(true);
		setResizable(true);
		setMovable(true);
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");

		addWindowPaneListener(new WindowPaneListener() {

			private static final long serialVersionUID = -8059169383151712617L;

			public void windowPaneClosing(WindowPaneEvent e) {
				actionPerformed(new ActionEvent(SimpleErrorMessageWindow.this, "Close"));
			}

		});

		HtmlLabel html = new HtmlLabel("<pre>" + message + "</pre>");
		html.setFont(new Font(Font.MONOSPACE, Font.PLAIN, new Extent(12)));

		Grid grid = new Grid(1);
		grid.setInsets(new Insets(5, 5, 5, 5));
		grid.add(html);
		add(grid);
	}


	public void actionPerformed(ActionEvent e) {
		setVisible(false);
	}

	private String localize(String s) {
		String text = LocaleResources.getString(s);
		if (text == null) text = s;
		return text;
	}
}