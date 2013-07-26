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

import nextapp.echo.app.Extent;
import nextapp.echo.app.WindowPane;

/**
 * This class represents a general Echo window.
 * 
 * @author Kaarel Kaljurand
 */
public class GeneralWindow extends WindowPane {

	private static final long serialVersionUID = 3862130026472878855L;

	/**
	 * Center this window with respect to the given parent.
	 * 
	 * @param parent The parent window.
	 */
	public void setCentered(WindowPane parent) {
		if (parent != null && parent.getPositionX() != null) {
			int px = parent.getPositionX().getValue();
			int py = parent.getPositionY().getValue();
			int pw = parent.getWidth().getValue();
			int ph = parent.getHeight().getValue();
			setPositionX(new Extent(px + (pw - getWidth().getValue())/2));
			setPositionY(new Extent(py + (ph - getHeight().getValue())/2));
		}
	}

}
