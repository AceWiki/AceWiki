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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.EditorController;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.base.ConcreteOption;
import ch.uzh.ifi.attempto.base.NextTokenOptions;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.base.TextOperator;
import ch.uzh.ifi.attempto.preditor.DefaultMenuItemComparator;
import ch.uzh.ifi.attempto.preditor.MenuCreator;
import ch.uzh.ifi.attempto.preditor.MenuEntry;
import ch.uzh.ifi.attempto.preditor.MenuItem;
import ch.uzh.ifi.attempto.preditor.SpecialMenuItem;

/**
 * This is the menu creator class that generates the menu entries for the predictive editor
 * on the basis of the AceWiki grammar.
 * 
 * @author Tobias Kuhn
 */
public class AceWikiMenuCreator implements MenuCreator, ActionListener {
	
	private static final long serialVersionUID = -6442603864805781298L;
	
	private Wiki wiki;
	private OntologyElement highlightedElement;
	private ActionListener actionListener;
	private DefaultMenuItemComparator comparator = new DefaultMenuItemComparator();
	
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
	}
	
	private EditorController getEditorController() {
		return wiki.getLanguageHandler().getEditorController();
	}
	
	public List<String> getMenuGroupOrdering() {
		return getEditorController().getMenuGroups();
	}

	public MenuEntry createMenuEntry(ConcreteOption option) {
		String menuGroup = getEditorController().getMenuGroup(option.getCategoryName());
		TextOperator to = wiki.getLanguageHandler().getTextOperator();
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
		for (String p : getEditorController().getExtensibleCategories()) {
			if (!options.containsCategory(p)) continue;
			String g = getEditorController().getMenuGroup(p);
			menuItems.add(new SpecialMenuItem(wiki.getGUIText("acewiki_preditor_menuitemnew"), g, p, this));
		}
		return menuItems;
	}
	
	public void actionPerformed(ActionEvent e) {
		String p = e.getActionCommand();
		String type = getEditorController().getWordType(p);
		int wordNumber = getEditorController().getWordNumber(p);
		wiki.showCreatorWindow(type, wordNumber, actionListener);
	}

	public int getColorShift(String menuBlockName) {
		return getEditorController().getColorShift(menuBlockName);
	}

	public Comparator<MenuItem> getMenuItemComparator() {
		return comparator;
	}

}
