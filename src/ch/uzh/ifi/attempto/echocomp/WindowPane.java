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

/**
 * This class creates window panes that arrange themselves on the z-axis in a way that
 * newer window panes are displayed in front of older ones.
 * 
 * @author Tobias Kuhn
 */
public class WindowPane extends nextapp.echo2.app.WindowPane {

	private static final long serialVersionUID = 3451270691632105615L;
	
	private static int zIndex = 10;

	/**
	 * Creates a new window pane.
	 */
	public WindowPane() {
		super();
		int z = zIndex;
		if (z < 0) {
			zIndex = 10;
			z = zIndex;
		}
		zIndex++;
		setZIndex(z);
	}

}

