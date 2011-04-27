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

import java.util.List;

import nextapp.echo.app.Color;
import nextapp.echo.app.Row;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.aceowl.ProperNameIndividual;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.preditor.TextElement;

public class TextRow extends Row {
	
	public TextRow(List<TextElement> text, Wiki wiki, boolean isRed) {
		Color color = Color.BLACK;
		if (isRed) {
			color = new Color(180, 0, 0);
		}
		for (TextElement e : text) {
			if (!e.getText().matches("[.?]") && getComponentCount() > 0) {
				add(new HSpace());
			}
			if (e instanceof OntologyTextElement) {
				OntologyTextElement ote = (OntologyTextElement) e;
				OntologyElement oe = ote.getOntologyElement();
				if (oe instanceof ProperNameIndividual) {
					// Proper names with definite articles are handled differently: The "the" is
					// not a part of the link.
					// TODO: This should be done at a different place!
					ProperNameIndividual ind = (ProperNameIndividual) oe;
					int wn = ote.getWordNumber();
					if (ind.hasDefiniteArticle(wn)) {
						SolidLabel l = new SolidLabel(e.getText().substring(0, 3));
						l.setForeground(color);
						add(l);
						add(new HSpace());
						add(new WikiLink(oe, oe.getPrettyWord(wn + 1), wiki, isRed));
					} else {
						add(new WikiLink(((OntologyTextElement) e), wiki, isRed));
					}
				} else {
					add(new WikiLink(((OntologyTextElement) e), wiki, isRed));
				}
			} else {
				SolidLabel l = new SolidLabel(e.getText());
				l.setForeground(color);
				add(l);
			}
		}
	}

}
