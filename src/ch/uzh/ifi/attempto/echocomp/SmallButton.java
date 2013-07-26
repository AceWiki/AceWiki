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
import nextapp.echo.app.Button;
import nextapp.echo.app.Color;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.event.ActionListener;

/**
 * This class creates borderless small buttons. 
 *  
 * @author Tobias Kuhn
 */
public class SmallButton extends Button {
	
	private static final long serialVersionUID = 1529104452172147464L;

	/**
	 * Creates a new small button.
	 * 
	 * @param s Either the text key for localization or the actual button text.
	 * @param actionListener The action-listener of the button.
	 * @param size The size of the text.
	 */
	public SmallButton(String s, ActionListener actionListener, int size) {
		String text = LocaleResources.getString(s);
		if (text == null) text = s;
		setText(text);
		
		setActionCommand(s);
		addActionListener(actionListener);
		
		setHeight(new Extent(size + 4));
		setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(size)));
		setBackground(null);
		setForeground(Style.mediumForeground);
		setDisabledBackground(null);
		setDisabledForeground(Color.BLACK);
		
		setRolloverEnabled(true);
		setRolloverForeground(Style.lightForeground);
		setRolloverBackground(Style.darkBackground);
		setInsets(new Insets(2, 0));
		setAlignment(new Alignment(Alignment.CENTER, Alignment.CENTER));
		setTextAlignment(new Alignment(Alignment.CENTER, Alignment.CENTER));
		
		setLineWrap(false);
	}
	
	/**
	 * Creates a new small button with text size 10.
	 * 
	 * @param s Either the text key for localization or the actual button text.
	 * @param actionListener The action-listener of the button.
	 */
	public SmallButton(String s, ActionListener actionListener) {
		this(s, actionListener, 10);
	}

}
