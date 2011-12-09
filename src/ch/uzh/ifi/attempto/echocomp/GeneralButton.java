// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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
import nextapp.echo.app.Border;
import nextapp.echo.app.Button;
import nextapp.echo.app.Color;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.event.ActionListener;

/**
 * This is a convenience class for easy creation of buttons.
 * 
 * @author Tobias Kuhn
 */
public class GeneralButton extends Button {

	private static final long serialVersionUID = -1385798331737572623L;

	/**
	 * Creates a new button.
	 * 
	 * @param text The button text.
	 * @param width The button width.
	 * @param actionListener The action-listener of the button.
	 */
	public GeneralButton(String text, int width, ActionListener actionListener) {
		super(text);
		
		if (width > 0) {setBorder(new Border(1, Color.BLACK, Border.STYLE_OUTSET));
			setWidth(new Extent(width));
		}
		setHeight(new Extent(17));
		setBackground(Style.mediumBackground);
		setForeground(Style.darkForeground);
		setBorder(new Border(1, Color.BLACK, Border.STYLE_OUTSET));
		setDisabledBackground(Style.lightDisabled);
		setDisabledForeground(Style.darkDisabled);
		setDisabledBorder(new Border(1, Style.darkDisabled, Border.STYLE_OUTSET));
		setRolloverEnabled(true);
		setRolloverForeground(Style.lightForeground);
		setRolloverBackground(Style.darkBackground);
		setRolloverBorder(new Border(1, Color.BLACK, Border.STYLE_SOLID));
		setInsets(new Insets(2, 0));
		setAlignment(new Alignment(Alignment.CENTER, Alignment.CENTER));
		setTextAlignment(new Alignment(Alignment.CENTER, Alignment.CENTER));
		setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		
		setActionCommand(text);
		addActionListener(actionListener);
	}
	
	/**
	 * Creates a new button.
	 * 
	 * @param text The button text.
	 * @param actionListener The action-listener of the button.
	 */
	public GeneralButton(String text, ActionListener actionListener) {
		this(text, 0, actionListener);
	}
	
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		
		// There seems to be a bug in Echo: the disabled background color is not displayed
		// properly. This is a workaround for this Problem:
		if (enabled) {
			setBackground(Style.mediumBackground);
		} else {
			setBackground(null);
		}
	}

}
