// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

import java.util.List;

import nextapp.echo.app.Color;
import nextapp.echo.app.Row;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;

/**
 * This component renders a CNL text into GUI elements.
 * 
 * @author Tobias Kuhn
 */
public class TextRow extends Row {
	
	private static final long serialVersionUID = -3410891086679856030L;
	
	/**
	 * Creates a new text row with the given text.
	 * 
	 * @param text The text as a list of text elements.
	 * @param wiki The wiki object.
	 * @param isRed true if the text color should be red.
	 */
	public TextRow(List<TextElement> text, Wiki wiki, boolean isRed) {
		Color color = Color.BLACK;
		if (isRed) {
			color = new Color(180, 0, 0);
		}
		TextElement prev = null;
		for (TextElement e : text) {
			if (prev != null) {
				String glue = wiki.getLanguageHandler().getTextOperator().getGlue(prev, e);
				if (glue.matches("\\s+")) {
					add(new HSpace(5 * glue.length()));
				} else if (glue.length() > 0) {
					add(new SolidLabel(glue));
				}
			}
			prev = e;
			if (e instanceof OntologyTextElement) {
				add(new WikiLink(((OntologyTextElement) e), wiki, isRed));
			} else {
				SolidLabel l = new SolidLabel(e.getText());
				l.setForeground(color);
				add(l);
			}
		}
	}

	/**
	 * Creates a new text row with the given text.
	 * 
	 * @param text The text as a list of text elements.
	 * @param wiki The wiki object.
	 */
	public TextRow(List<TextElement> text, Wiki wiki) {
		this(text, wiki, false);
	}

}
