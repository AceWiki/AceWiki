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

/**
 * This is a convenience class for easy creation of labels.
 * 
 * @author Tobias Kuhn
 */
public class Label extends nextapp.echo.app.Label {
	
	private static final long serialVersionUID = -1013262375038053365L;

	/**
	 * Creates a new empty label.
	 */
	public Label() {
		super();
		initLabel(Font.PLAIN, 13);
	}
	
	/**
	 * Creates a new label containing only an image.
	 * 
	 * @param image The image.
	 */
	public Label(ImageReference image) {
		super(image);
		initLabel(Font.PLAIN, 13);
	}
	
	/**
	 * Creates a new label.
	 * 
	 * @param text The text of the label.
	 */
	public Label(String text) {
		super(text);
		initLabel(Font.PLAIN, 13);
	}
	
	/**
	 * Creates a new label of the given style.
	 * 
	 * @param text The text of the label.
	 * @param style The style of the label.
	 */
	public Label(String text, int style) {
		super(text);
		initLabel(style, 13);
	}
	
	/**
	 * Creates a new label of the given style with the given text size.
	 * 
	 * @param text The text of the label.
	 * @param style The style of the label.
	 * @param size The size of the text.
	 */
	public Label(String text, int style, int size) {
		super(text);
		initLabel(style, size);
	}
	
	void initLabel(int style, int size) {
		setFont(new Font(Style.fontTypeface, style, new Extent(size)));
	}

}
