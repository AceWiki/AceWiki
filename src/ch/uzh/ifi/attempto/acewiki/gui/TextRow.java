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

import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Row;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextContainerSet;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;

/**
 * This component renders a CNL text into GUI elements.
 * 
 * @author Tobias Kuhn
 * @author Kaarel Kaljurand
 */
public class TextRow extends Column {

	private static final long serialVersionUID = -3410891086679856030L;

	/**
	 * Constructs a column of text rows, where each row corresponds to
	 * an element of the given set of text containers.
	 * 
	 * @param textContainerSet Set of text containers.
	 * @param wiki The wiki object.
	 * @param isRed true if the text color should be red.
	 */
	// TODO: maybe apply the color to the complete column, not individually to each label
	public TextRow(TextContainerSet textContainerSet, Wiki wiki, boolean isRed) {
		Color color = Color.BLACK;
		if (isRed) {
			color = new Color(180, 0, 0);
		}
		for (TextContainer tc : textContainerSet) {
			Row row = new Row();
			TextElement prev = null;
			for (TextElement e : tc.getTextElements()) {
				if (prev != null) {
					String glue = wiki.getLanguageHandler().getTextOperator().getGlue(prev, e);
					if (glue.matches("\\s+")) {
						row.add(new HSpace(5 * glue.length()));
					} else if (glue.length() > 0) {
						row.add(new SolidLabel(glue));
					}
				}
				prev = e;
				if (e instanceof OntologyTextElement) {
					row.add(new WikiLink(((OntologyTextElement) e), wiki, isRed));
				} else {
					SolidLabel l = new SolidLabel(e.getText());
					l.setForeground(color);
					row.add(l);
				}
			}
			add(row);
		}
	}

	/**
	 * Constructs a column of text rows, where each row corresponds to
	 * an element of the given set of text containers.
	 * 
	 * @param textContainerSet Set of text containers.
	 * @param wiki The wiki object.
	 */
	public TextRow(TextContainerSet textContainerSet, Wiki wiki) {
		this(textContainerSet, wiki, false);
	}

}
