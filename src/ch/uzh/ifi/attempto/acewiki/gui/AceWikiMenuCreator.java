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

import java.util.ArrayList;
import java.util.List;

import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.MenuEngine;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.chartparser.ConcreteOption;
import ch.uzh.ifi.attempto.chartparser.NextTokenOptions;
import ch.uzh.ifi.attempto.preditor.DefaultMenuCreator;
import ch.uzh.ifi.attempto.preditor.MenuEntry;
import ch.uzh.ifi.attempto.preditor.SpecialMenuItem;
import ch.uzh.ifi.attempto.preditor.TextElement;
import ch.uzh.ifi.attempto.preditor.TextOperator;

/**
 * This is the menu creator class that generates the menu entries for the predictive editor
 * on the basis of the AceWiki grammar.
 * 
 * @author Tobias Kuhn
 */
public class AceWikiMenuCreator extends DefaultMenuCreator implements ActionListener {
	
	private static final long serialVersionUID = -6442603864805781298L;
	
	private Wiki wiki;
	private OntologyElement highlightedElement;
	private ActionListener actionListener;
	
	/**
	 * Creates a new AceWiki-specific menu creator object.
	 * 
	 * @param wiki The wiki instance.
	 * @param highlightedElement The ontology element that should be highlighted in the editor
	 *     (because it is the current element).
	 * @param actionListener The action-listener.
	 */
	public AceWikiMenuCreator(Wiki wiki, OntologyElement highlightedElement,
			ActionListener actionListener) {
		this.wiki = wiki;
		this.highlightedElement = highlightedElement;
		this.actionListener = actionListener;
		
		for (String s : getMenuEngine().getMenuGroups()) {
			setColorShift(s, getMenuEngine().getColorShift(s));
		}
	}
	
	private MenuEngine getMenuEngine() {
		return wiki.getLanguageEngine().getMenuEngine();
	}
	
	public List<String> getMenuGroupOrdering() {
		return getMenuEngine().getMenuGroups();
	}

	public MenuEntry createMenuEntry(ConcreteOption option) {
		String menuGroup = getMenuEngine().getMenuGroup(option.getCategoryName());
		String w = option.getWord();
		TextOperator to = wiki.getOntology().getTextOperator();
		TextElement te = to.createTextElement(option.getWord());
		MenuEntry me = new MenuEntry(te, menuGroup);
		if (te instanceof OntologyTextElement) {
			OntologyTextElement ote = (OntologyTextElement) te;
			me.setHighlighted(ote.getOntologyElement() == highlightedElement);
		}
		return me;
	}

	public List<SpecialMenuItem> createSpecialMenuItems(NextTokenOptions options) {
		List<SpecialMenuItem> menuItems = new ArrayList<SpecialMenuItem>();
		for (String p : getMenuEngine().getExtensibleCategories()) {
			if (!options.containsPreterminal(p)) continue;
			menuItems.add(new SpecialMenuItem("new...", getMenuEngine().getMenuGroup(p), p, this));
		}
		return menuItems;
	}
	
	public void actionPerformed(ActionEvent e) {
		String p = e.getActionCommand();
		String type = getMenuEngine().getWordType(p);
		int wordNumber = getMenuEngine().getWordNumber(p);
		wiki.showCreatorWindow(type, wordNumber, actionListener);
	}

}
