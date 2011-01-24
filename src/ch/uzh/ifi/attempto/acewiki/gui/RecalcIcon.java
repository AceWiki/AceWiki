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

package ch.uzh.ifi.attempto.acewiki.gui;

import nextapp.echo.app.ResourceImageReference;
import ch.uzh.ifi.attempto.echocomp.Label;

/**
 * This class represents an icon that is shown when a reasoning result is begin recalculated.
 * 
 * @author Tobias Kuhn
 */
public class RecalcIcon extends Label {

	private static final long serialVersionUID = -8184986319413801322L;
	
	/**
	 * Creates a new recalculation icon with the given tool tip text.
	 * 
	 * @param toolTipText The tool tip text.
	 */
	public RecalcIcon(String toolTipText) {
		super(new ResourceImageReference("ch/uzh/ifi/attempto/acewiki/gui/img/recalc.png"));
		if (toolTipText != null) {
			setToolTipText(toolTipText);
		}
	}
	
	/**
	 * Creates a new recalculation icon without tool tip.
	 */
	public RecalcIcon() {
		this(null);
	}

}
