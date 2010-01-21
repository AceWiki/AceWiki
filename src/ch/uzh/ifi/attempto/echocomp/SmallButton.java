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

import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Button;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.event.ActionListener;

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
	 * @param text The button text.
	 * @param actionListener The action-listener of the button.
	 * @param size The size of the text.
	 * @param enabled false if the button should be disabled.
	 */
	public SmallButton(String text, ActionListener actionListener, int size, boolean enabled) {
		super(text);
		
		setActionCommand(text);
		addActionListener(actionListener);
		
		setHeight(new Extent(size + 2));
		setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(size)));
		setBackground(null);
		setForeground(Style.mediumForeground);
		setDisabledBackground(null);
		setDisabledForeground(Color.BLACK);
		setActionCommand(text);
		
		setRolloverEnabled(true);
		setRolloverForeground(Style.lightForeground);
		setRolloverBackground(Style.darkBackground);
		setInsets(new Insets(2, 1));
		setAlignment(new Alignment(Alignment.CENTER, Alignment.CENTER));
		setTextAlignment(new Alignment(Alignment.CENTER, Alignment.CENTER));
		
		setEnabled(enabled);
		setLineWrap(false);
	}
	
	/**
	 * Creates a new small button.
	 * 
	 * @param text The button text.
	 * @param actionListener The action-listener of the button.
	 * @param size The size of the text.
	 */
	public SmallButton(String text, ActionListener actionListener, int size) {
		this(text, actionListener, size, true);
	}
	
	/**
	 * Creates a new small button with text size 10.
	 * 
	 * @param text The button text.
	 * @param actionListener The action-listener of the button.
	 * @param enabled false if the button should be disabled.
	 */
	public SmallButton(String text, ActionListener actionListener, boolean enabled) {
		this(text, actionListener, 10, enabled);
	}
	
	/**
	 * Creates a new small button with text size 10.
	 * 
	 * @param text The button text.
	 * @param actionListener The action-listener of the button.
	 */
	public SmallButton(String text, ActionListener actionListener) {
		this(text, actionListener, 10, true);
	}

}
