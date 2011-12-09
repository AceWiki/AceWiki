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

import nextapp.echo.app.Column;
import nextapp.echo.app.Insets;

/**
 * This class produces a vertical empty space.
 * 
 * @author Tobias Kuhn
 */
public class VSpace extends Column {

	private static final long serialVersionUID = 6466987710837687895L;

	/**
	 * Creates a vertical empty space of default size 5.
	 */
	public VSpace() {
		this(5);
	}

	/**
	 * Creates a vertical empty space of the given size.
	 * 
	 * @param size The size.
	 */
	public VSpace(int size) {
		super();
		setInsets(new Insets(0, size, 0, 0));
	}

}
