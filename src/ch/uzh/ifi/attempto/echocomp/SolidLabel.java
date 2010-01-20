// This file is part of the Attempto Java Packages.
// Copyright 2008-2009, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Java Packages is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// The Attempto Java Packages is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Java Packages. If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.echocomp;

import nextapp.echo2.app.ImageReference;

/**
 * This is a convenience class for easy creation of labels with no line wrap.
 * 
 * @author Tobias Kuhn
 */
public class SolidLabel extends Label {

	private static final long serialVersionUID = -1721467828910909396L;

	/**
	 * Creates a new empty label.
	 */
	public SolidLabel() {
		super();
	}

	/**
	 * Creates a new label containing only an image.
	 * 
	 * @param image The image.
	 */
	public SolidLabel(ImageReference image) {
		super(image);
	}

	/**
	 * Creates a new label.
	 * 
	 * @param text The text of the label.
	 */
	public SolidLabel(String text) {
		super(text);
	}

	/**
	 * Creates a new label of the given style.
	 * 
	 * @param text The text of the label.
	 * @param style The style of the label.
	 */
	public SolidLabel(String text, int style) {
		super(text, style);
	}

	/**
	 * Creates a new label of the given style with the given text size.
	 * 
	 * @param text The text of the label.
	 * @param style The style of the label.
	 * @param size The size of the text.
	 */
	public SolidLabel(String text, int style, int size) {
		super(text, style, size);
	}
	
	void initLabel(int style, int size) {
		super.initLabel(style, size);
		setLineWrap(false);
	}

}
