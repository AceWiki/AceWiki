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

package ch.uzh.ifi.attempto.acewiki.aceowl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.OntologyTextElement;
import ch.uzh.ifi.attempto.acewiki.gui.FormPane;
import ch.uzh.ifi.attempto.chartparser.ConcreteOption;
import ch.uzh.ifi.attempto.chartparser.NextTokenOptions;
import ch.uzh.ifi.attempto.preditor.DefaultMenuCreator;
import ch.uzh.ifi.attempto.preditor.MenuEntry;
import ch.uzh.ifi.attempto.preditor.SpecialMenuItem;
import ch.uzh.ifi.attempto.preditor.TextOperator;

/**
 * This is the menu creator class that generates the menu entries for the predictive editor
 * on the basis of the AceWiki grammar.
 * 
 * @author Tobias Kuhn
 */
public class ACEOWLMenuCreator extends DefaultMenuCreator implements ActionListener {
	
	private static final long serialVersionUID = -6442603864805781298L;
	
	private static Map<String, String> cats;
	private static List<String> menuGroupOrdering;
	
	private Wiki wiki;
	private OntologyElement highlightedElement;
	private ActionListener actionListener;

	static {
		cats = new HashMap<String, String>();
		cats.put("propername", "proper name");
		cats.put("noun", "noun");
		cats.put("defnoun", "reference");
		cats.put("nounpl", "plural noun");
		cats.put("nounof", "of-construct");
		cats.put("verbsg", "verb");
		cats.put("verbinf", "verb");
		cats.put("pverb", "passive verb");
		cats.put("tradj", "transitive adjective");

		menuGroupOrdering = new ArrayList<String>();
		menuGroupOrdering.add("function word");
		menuGroupOrdering.add("proper name");
		menuGroupOrdering.add("noun");
		menuGroupOrdering.add("plural noun");
		menuGroupOrdering.add("of-construct");
		menuGroupOrdering.add("transitive adjective");
		menuGroupOrdering.add("verb");
		menuGroupOrdering.add("passive verb");
		menuGroupOrdering.add("new variable");
		menuGroupOrdering.add("reference");
	}
	
	/**
	 * Creates a new AceWiki-specific menu creator object.
	 * 
	 * @param wiki The wiki instance.
	 * @param highlightedElement The ontology element that should be highlighted in the editor
	 *     (because it is the current element).
	 * @param actionListener The action-listener.
	 */
	public ACEOWLMenuCreator(Wiki wiki, OntologyElement highlightedElement,
			ActionListener actionListener) {
		this.wiki = wiki;
		this.highlightedElement = highlightedElement;
		this.actionListener = actionListener;

		setColorShift("proper name", 60);
		setColorShift("noun", 100);
		setColorShift("plural noun", 120);
		setColorShift("of-construct", 140);
		setColorShift("transitive adjective", 180);
		setColorShift("verb", 210);
		setColorShift("passive verb", 210);
		setColorShift("new variable", 320);
		setColorShift("reference", 320);
	}
	
	public List<String> getMenuGroupOrdering() {
		return menuGroupOrdering;
	}

	public MenuEntry createMenuEntry(ConcreteOption option) {
		String n = option.getCategoryName();
		String w = option.getWord();
		if (n == null) {
			return new MenuEntry(option, "function word");
		} else if (n.equals("variable")) {
			return new MenuEntry(option, "new variable");
		} else if (n.equals("reference")) {
			return new MenuEntry(option, "reference");
		} else if (n.equals("number")) {
			return new MenuEntry(option, "function word");
		} else if (cats.containsKey(n)) {
			try {
				// TODO: do this at a different place!
				TextOperator to = wiki.getOntology().getTextOperator();
				OntologyTextElement ote = (OntologyTextElement) to.createTextElement(w);
				MenuEntry me = new MenuEntry(ote, cats.get(n));
				me.setHighlighted(ote.getOntologyElement() == highlightedElement);
				return me;
			} catch (ClassCastException ex) {}
		}
		return new MenuEntry(option, "function word");
	}

	public List<SpecialMenuItem> createSpecialMenuItems(NextTokenOptions options) {
		List<SpecialMenuItem> menuItems = new ArrayList<SpecialMenuItem>();
		if (options.containsPreterminal("propername")) {
			menuItems.add(new SpecialMenuItem("new...", "proper name", "propername/0", this));
		}
		if (options.containsPreterminal("noun")) {
			menuItems.add(new SpecialMenuItem("new...", "noun", "noun/0", this));
		}
		if (options.containsPreterminal("nounpl")) {
			menuItems.add(new SpecialMenuItem("new...", "plural noun", "noun/1", this));
		}
		if (options.containsPreterminal("nounof")) {
			menuItems.add(new SpecialMenuItem("new...", "of-construct", "nounof/0", this));
		}
		if (options.containsPreterminal("verbsg")) {
			menuItems.add(new SpecialMenuItem("new...", "verb", "trverb/0", this));
		}
		if (options.containsPreterminal("verbinf")) {
			menuItems.add(new SpecialMenuItem("new...", "verb", "trverb/1", this));
		}
		if (options.containsPreterminal("pverb")) {
			menuItems.add(new SpecialMenuItem("new...", "passive verb", "trverb/2", this));
		}
		if (options.containsPreterminal("tradj")) {
			menuItems.add(new SpecialMenuItem("new...", "transitive adjective", "tradj/0", this));
		}
		return menuItems;
	}
	
	public void actionPerformed(ActionEvent e) {
		String[] s = e.getActionCommand().split("/");
		String type = s[0];
		int wordNumber = new Integer(s[1]);
		wiki.showWindow(FormPane.createCreatorWindow(type, wordNumber, wiki, actionListener));
	}

}
