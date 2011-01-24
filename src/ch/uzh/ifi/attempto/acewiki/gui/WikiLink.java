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

import nextapp.echo.app.Button;
import nextapp.echo.app.Color;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.DummyOntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyTextElement;
import ch.uzh.ifi.attempto.echocomp.Style;

/**
 * This class represents a wiki-internal link.
 * 
 * @author Tobias Kuhn
 */
public class WikiLink extends Button implements ActionListener {
	
	private static final long serialVersionUID = -2234400779848457043L;
	
	private OntologyElement ontologyElement;
	private Wiki wiki;
	
	/**
	 * Creates a new link to the article of the given ontology element. The headword of the
	 * ontology element is used as the link text.
	 * 
	 * @param ontologyElement The ontology element whose article should be linked.
	 * @param wiki The wiki instance.
	 */
	public WikiLink(OntologyElement ontologyElement, Wiki wiki) {
		super(ontologyElement.getHeadword());
		this.wiki = wiki;
		this.ontologyElement = ontologyElement;
		initButton(false);
	}
	
	/**
	 * Creates a new link to the article of the given ontology element.
	 * 
	 * @param ontologyElement The ontology element whose article should be linked.
	 * @param text The link text.
	 * @param wiki The wiki instance.
	 * @param red true if the link text should be displayed in red font.
	 */
	public WikiLink(OntologyElement ontologyElement, String text, Wiki wiki, boolean red) {
		super(text);
		this.wiki = wiki;
		this.ontologyElement = ontologyElement;
		initButton(red);
	}
	
	/**
	 * Creates a new wiki link on the basis of a text element. It links to the article of the
	 * ontology element of the text element. The text of the text element is used as the link
	 * text.
	 * 
	 * @param textElement The text element.
	 * @param wiki The wiki instance.
	 * @param red true if the link text should be displayed in red font.
	 */
	public WikiLink(OntologyTextElement textElement, Wiki wiki, boolean red) {
		super(textElement.getText());
		this.wiki = wiki;
		this.ontologyElement = textElement.getOntologyElement();
		initButton(red);
	}
	
	private void initButton(boolean red) {
		setInsets(new Insets(0, 0, 0, 0));
		setLineWrap(false);
		setRolloverEnabled(true);
		if (ontologyElement instanceof DummyOntologyElement) {
			setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		} else {
			setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(13)));
		}
		setRolloverForeground(Color.BLUE);
		if (red) setForeground(new Color(180, 0, 0));
		addActionListener(this);
	}

	public void actionPerformed(ActionEvent e) {
		wiki.log("page", "pressed: link " + ontologyElement.getWord());
		wiki.showPage(ontologyElement);
	}

}
