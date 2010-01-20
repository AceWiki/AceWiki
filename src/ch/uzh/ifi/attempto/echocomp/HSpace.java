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

import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;

/**
 * This class produces a horizontal empty space.
 * 
 * @author Tobias Kuhn
 */
public class HSpace extends Row {
	
	private static final long serialVersionUID = 961641712269003259L;

	/**
	 * Creates a horizontal empty space of default size 5.
	 */
	public HSpace() {
		this(5);
	}
	
	/**
	 * Creates a horizontal empty space of the given size.
	 * 
	 * @param size The size.
	 */
	public HSpace(int size) {
		super();
		setInsets(new Insets(size, 0, 0, 0));
	}

}
