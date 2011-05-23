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
import nextapp.echo.app.ResourceImageReference;

/**
 * This class represents a check box in blue style.
 * 
 * @author Tobias Kuhn
 */
public class CheckBox extends nextapp.echo.app.CheckBox {

	private static final long serialVersionUID = -8160475963811004744L;
	
	private static final String iconPath = "ch/uzh/ifi/attempto/echocomp/style/";

	/**
	 * Creates a new check box having a text and an icon.
	 * 
	 * @param text The text.
	 * @param icon The icon.
	 */
	public CheckBox(String text, ImageReference icon) {
		super(text, icon);
		setStateIcon(Style.getImage(iconPath + "notchecked.png"));
		setSelectedStateIcon(Style.getImage(iconPath + "checked.png"));
		setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(13)));
	}
	
	/**
	 * Creates a new check box having only a text.
	 * 
	 * @param text The text.
	 */
	public CheckBox(String text) {
		this(text, null);
	}
	
	/**
	 * Creates a new check box having only an icon.
	 * 
	 * @param icon The icon.
	 */
	public CheckBox(ImageReference icon) {
		this(null, icon);
	}
	
	/**
	 * Creates a new check box having neither a text nor an icon.
	 */
	public CheckBox() {
		this(null, null);
	}
	
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		if (enabled) {
			setStateIcon(new ResourceImageReference(iconPath + "notchecked.png"));
			setSelectedStateIcon(new ResourceImageReference(iconPath + "checked.png"));
		} else {
			setStateIcon(new ResourceImageReference(iconPath + "notcheckedi.png"));
			setSelectedStateIcon(new ResourceImageReference(iconPath + "checkedi.png"));
		}
	}

}
