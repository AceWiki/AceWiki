// This file is part of AceWiki.
// Copyright 2008-2011, Tobias Kuhn.
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

import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.ImageReference;
import nextapp.echo.app.button.ButtonGroup;

/**
 * This class represents a radio button in blue style.
 * 
 * @author Tobias Kuhn
 */
public class RadioButton extends nextapp.echo.app.RadioButton {
	
	private static final long serialVersionUID = 1429240270043389676L;

	/**
	 * Creates a new radio button having a text and an icon.
	 * 
	 * @param text The text.
	 * @param icon The icon.
	 * @param group The button group.
	 */
	public RadioButton(String text, ImageReference icon, ButtonGroup group) {
		super(text, icon);
		if (group != null) setGroup(group);
		setStateIcon(Style.getImage("ch/uzh/ifi/attempto/echocomp/style/radiooff.png"));
		setSelectedStateIcon(Style.getImage("ch/uzh/ifi/attempto/echocomp/style/radioon.png"));
		setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(13)));
	}
	
	/**
	 * Creates a new radio button having only a text.
	 * 
	 * @param text The text.
	 * @param group The button group.
	 */
	public RadioButton(String text, ButtonGroup group) {
		this(text, null, group);
	}
	
	/**
	 * Creates a new radio button having only an icon.
	 * 
	 * @param icon The icon.
	 * @param group The button group.
	 */
	public RadioButton(ImageReference icon, ButtonGroup group) {
		this(null, icon, group);
	}
	
	/**
	 * Creates a new radio button having neither a text nor an icon.
	 * 
	 * @param group The button group.
	 */
	public RadioButton(ButtonGroup group) {
		this(null, null, group);
	}
	
	/**
	 * Creates a new radio button having only a text.
	 * 
	 * @param text The text.
	 */
	public RadioButton(String text) {
		this(text, null, null);
	}
	
	/**
	 * Creates a new radio button having only an icon.
	 * 
	 * @param icon The icon.
	 */
	public RadioButton(ImageReference icon) {
		this(null, icon, null);
	}
	
	/**
	 * Creates a new radio button having neither a text nor an icon.
	 */
	public RadioButton() {
		this(null, null, null);
	}

}
