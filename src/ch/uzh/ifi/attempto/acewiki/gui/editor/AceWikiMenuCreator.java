// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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

package ch.uzh.ifi.attempto.acewiki.gui.editor;

import java.util.ArrayList;
import java.util.List;

import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Individual;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyTextElement;
import ch.uzh.ifi.attempto.chartparser.ConcreteOption;
import ch.uzh.ifi.attempto.chartparser.NextTokenOptions;
import ch.uzh.ifi.attempto.chartparser.Preterminal;
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
class AceWikiMenuCreator extends MenuCreator implements ActionListener {
	
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
		
		initializeMenuGroup("function word", true);
		initializeMenuGroup("noun", true);
		initializeMenuGroup("plural noun", true);
		initializeMenuGroup("proper name", true);
		initializeMenuGroup("of-construct", true);
		initializeMenuGroup("verb", true);
		initializeMenuGroup("passive verb", true);
		initializeMenuGroup("transitive adjective", true);
		initializeMenuGroup("reference", false);
		initializeMenuGroup("new variable", false);
	}

	public List<MenuItem> getMenuItems(NextTokenOptions options) {
		List<MenuItem> menuItems = new ArrayList<MenuItem>();
		
		for (ConcreteOption o : options.getConcreteOptions()) {
			menuItems.add(new MenuEntry(o, "function word"));
		}
		
		for (OntologyElement el : wiki.getOntologyElements()) {
			String t = el.getType();
			if (t.equals("Proper Name") && options.containsPreterminal("propername")) {
				menuItems.add(createMenuEntry(el, 0, "proper name"));
				if (((Individual) el).getAbbreviation() != null) {
					menuItems.add(createMenuEntry(el, 2, "proper name"));
				}
			} else if (t.equals("Noun")) {
				if (options.containsPreterminal("noun")) {
					menuItems.add(createMenuEntry(el, 0, "noun"));
				}
				if (options.containsPreterminal("defnoun")) {
					Preterminal cat = new Preterminal("defnoun");
					cat.setFeature("noun", el.getWord(0));
					cat.setFeature("text", "the " + el.getWord(0));
					OntologyTextElement te = new OntologyTextElement(el, 0, cat);
					te.setPreText("the ");
					menuItems.add(new MenuEntry(te, "reference"));
				}
				if (options.containsPreterminal("nounpl")) {
					menuItems.add(createMenuEntry(el, 1, "plural noun"));
				}
			} else if (t.equals("Of-Construct") && options.containsPreterminal("nounof")) {
				menuItems.add(createMenuEntry(el, 0, "of-construct"));
			} else if (t.equals("Verb")) {
				if (options.containsPreterminal("verbsg")) {
					menuItems.add(createMenuEntry(el, 0, "verb"));
				}
				if (options.containsPreterminal("verbinf")) {
					menuItems.add(createMenuEntry(el, 1, "verb"));
				}
				if (options.containsPreterminal("pverb") && el.getWord(2) != null) {
					menuItems.add(createMenuEntry(el, 2, "passive verb"));
				}
			} else if (t.equals("Transitive Adjective") && options.containsPreterminal("tradj")) {
				menuItems.add(createMenuEntry(el, 0, "transitive adjective"));
			}
		}
		
		if (options.containsPreterminal("variable")) {
			addVariableEntries(menuItems, "new variable", "variable");
		}
		
		if (options.containsPreterminal("reference")) {
			addVariableEntries(menuItems, "reference", "reference");
		}
		
		if (options.containsPreterminal("number")) {
			for (int i = 2 ; i < 100 ; i++) {
				menuItems.add(new MenuEntry(i + "", "number", "function word"));
			}
		}
		
		if (options.containsPreterminal("propername")) {
			menuItems.add(new SpecialMenuItem("new...", "proper name", "new propername", this));
		}
		if (options.containsPreterminal("noun")) {
			menuItems.add(new SpecialMenuItem("new...", "noun", "new noun", this));
		}
		if (options.containsPreterminal("nounpl")) {
			menuItems.add(new SpecialMenuItem("new...", "plural noun", "new nounpl", this));
		}
		if (options.containsPreterminal("nounof")) {
			menuItems.add(new SpecialMenuItem("new...", "of-construct", "new nounof", this));
		}
		if (options.containsPreterminal("verbsg")) {
			menuItems.add(new SpecialMenuItem("new...", "verb", "new verb", this));
		}
		if (options.containsPreterminal("verbinf")) {
			menuItems.add(new SpecialMenuItem("new...", "verb", "new verbinf", this));
		}
		if (options.containsPreterminal("pverb")) {
			menuItems.add(new SpecialMenuItem("new...", "passive verb", "new pverb", this));
		}
		if (options.containsPreterminal("tradj")) {
			menuItems.add(new SpecialMenuItem("new...", "transitive adjective", "new tradj", this));
		}
		return menuItems;
	}
	
	private static void addVariableEntries(List<MenuItem> entries, String menuBlockName,
			String categoryName) {
		String[] varNames = new String[] {
			"X", "Y", "Z", "X1", "Y1", "Z1", "X2", "Y2", "Z2",
			"X3", "Y3", "Z3", "X4", "Y4", "Z4", "X5", "Y5", "Z5"
		};
		for (String s : varNames) {
			Preterminal p = new Preterminal(categoryName);
			p.setFeature("text", s);
			entries.add(new MenuEntry(s, p, menuBlockName));
		}
	}
	
	private MenuEntry createMenuEntry(OntologyElement el, int wordNumber, String menuBlockName) {
		MenuEntry menuEntry = new MenuEntry(
				OntologyTextElement.createTextElement(el, wordNumber),
				menuBlockName
			);
		menuEntry.setHighlighted(el == highlightedElement);
		return menuEntry;
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("new propername")) {
			wiki.showWindow(ProperNameForm.createCreatorWindow(wiki, actionListener));
		} else if (e.getActionCommand().equals("new noun")) {
			wiki.showWindow(NounForm.createCreatorWindow(0, wiki, actionListener));
		} else if (e.getActionCommand().equals("new nounpl")) {
			wiki.showWindow(NounForm.createCreatorWindow(1, wiki, actionListener));
		} else if (e.getActionCommand().equals("new nounof")) {
			wiki.showWindow(NounOfForm.createCreatorWindow(wiki, actionListener));
		} else if (e.getActionCommand().equals("new verb")) {
			wiki.showWindow(VerbForm.createCreatorWindow(0, wiki, actionListener));
		} else if (e.getActionCommand().equals("new verbinf")) {
			wiki.showWindow(VerbForm.createCreatorWindow(1, wiki, actionListener));
		} else if (e.getActionCommand().equals("new pverb")) {
			wiki.showWindow(VerbForm.createCreatorWindow(2, wiki, actionListener));
		} else if (e.getActionCommand().equals("new tradj")) {
			wiki.showWindow(TrAdjForm.createCreatorWindow(wiki, actionListener));
		}
	}

}
