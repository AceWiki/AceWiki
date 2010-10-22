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

package ch.uzh.ifi.attempto.aceeditor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nextapp.echo2.app.Component;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.ape.FunctionWords;
import ch.uzh.ifi.attempto.chartparser.ConcreteOption;
import ch.uzh.ifi.attempto.chartparser.NextTokenOptions;
import ch.uzh.ifi.attempto.echocomp.CheckBox;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.preditor.MenuEntry;
import ch.uzh.ifi.attempto.preditor.PreditorWindow;
import ch.uzh.ifi.attempto.preditor.DefaultMenuCreator;
import ch.uzh.ifi.attempto.preditor.SpecialMenuItem;
import ch.uzh.ifi.attempto.preditor.WordEditorForm;
import ch.uzh.ifi.attempto.preditor.WordEditorWindow;

/**
 * This is the menu creator class for the ACE Editor.
 * 
 * @author Tobias Kuhn
 */
class ACEEditorMenuCreator extends DefaultMenuCreator implements ActionListener {

	private static final long serialVersionUID = -1357779780345075117L;
	
	private static Map<String, String> cats;
	private static List<String> extCats;
	private static List<String> menuGroupOrdering;
	private LexiconHandler lexiconHandler;
	private ACEEditor editor;
	private PreditorWindow preditorWindow;
	
	static {
		cats = new HashMap<String, String>();
		cats.put("noun_sg", "noun");
		cats.put("noun_pl", "noun");
		cats.put("prop_sg", "proper name");
		cats.put("propdef_sg", "proper name");
		cats.put("iv_finsg", "intransitive verb");
		cats.put("iv_infpl", "intransitive verb");
		cats.put("tv_finsg", "transitive verb");
		cats.put("tv_infpl", "transitive verb");
		cats.put("tv_pp", "passive verb");
		cats.put("adj_itr", "adjective");
		cats.put("adj_itr_comp", "adjective");
		cats.put("adj_itr_sup", "adjective");
		cats.put("adj_tr", "transitive adjective");
		cats.put("adj_tr_comp", "transitive adjective");
		cats.put("adv", "adverb");
		cats.put("adv_comp", "adverb");
		cats.put("adv_sup", "adverb");
		cats.put("prep", "preposition");
		extCats = new ArrayList<String>(cats.keySet());
		
		cats.put("adj_prep", "preposition");
		cats.put("pron", "reference");
		cats.put("def_noun_sg", "reference");
		cats.put("ref", "reference");
		cats.put("var", "new variable");
		cats.put("num", "function word");
		
		menuGroupOrdering = new ArrayList<String>();
		menuGroupOrdering.add("function word");
		menuGroupOrdering.add("proper name");
		menuGroupOrdering.add("noun");
		menuGroupOrdering.add("adjective");
		menuGroupOrdering.add("transitive adjective");
		menuGroupOrdering.add("intransitive verb");
		menuGroupOrdering.add("transitive verb");
		menuGroupOrdering.add("passive verb");
		menuGroupOrdering.add("adverb");
		menuGroupOrdering.add("preposition");
		menuGroupOrdering.add("new variable");
		menuGroupOrdering.add("reference");
	}

	ACEEditorMenuCreator(ACEEditor editor, LexiconHandler lexiconHandler) {
		this.editor = editor;
		this.lexiconHandler = lexiconHandler;
	}
	
	void setPreditorWindow(PreditorWindow preditorWindow) {
		this.preditorWindow = preditorWindow;
	}
	
	public List<String> getMenuGroupOrdering() {
		return menuGroupOrdering;
	}

	public MenuEntry createMenuEntry(ConcreteOption option) {
		String n = option.getCategoryName();
		if (n == null) {
			return new MenuEntry(option, "function word");
		} else if (cats.keySet().contains(n)) {
			return new MenuEntry(option, cats.get(n));
		} else {
			return new MenuEntry(option, "function word");
		}
	}

	public List<SpecialMenuItem> createSpecialMenuItems(NextTokenOptions options) {
		List<SpecialMenuItem> items = new ArrayList<SpecialMenuItem>();
		Map<String, String> m = new HashMap<String, String>();
		for (String s : extCats) {
			if (options.containsPreterminal(s)) {
				String menuGroup = cats.get(s);
				if (m.containsKey(menuGroup)) {
					m.put(menuGroup, m.get(menuGroup) + s + ":");
				} else {
					m.put(menuGroup, "new:" + s + ":");
				}
			}
		}
		if (!editor.isLexiconImmutable()) {
			for (String menuGroup : m.keySet()) {
				items.add(new SpecialMenuItem(
						"new...",
						menuGroup,
						m.get(menuGroup),
						this
					));
			}
		}
		return items;
	}
	
	public void actionPerformed(ActionEvent e) {
		
		String c = e.getActionCommand();
		if (c.startsWith("new:")) {
			WordEditorWindow wew = new WordEditorWindow("New Word", c, 500, 300);
			if (c.matches(".*:prop.*")) {
				WordEditorForm pane = new WordEditorForm("Proper Name", wew, this);
				pane.addRow("proper name", new TextField(), "", true);
				pane.addRow("... used with \"the\"", new CheckBox(), "", false);
				wew.addTab(pane);
				editor.showWindow(wew);
			} else if (c.matches(".*:noun_.*")) {
				WordEditorForm pane = new WordEditorForm("Noun", wew, this);
				pane.addRow("singular", new TextField(), "", true);
				pane.addRow("plural", new TextField(), "", true);
				wew.addTab(pane);
				editor.showWindow(wew);
			} else if (c.matches(".*:iv_.*")) {
				WordEditorForm pane = new WordEditorForm("Intransitive Verb", wew, this);
				pane.addRow("third singular", new TextField(), "", true);
				pane.addRow("bare infinitive", new TextField(), "", true);
				wew.addTab(pane);
				editor.showWindow(wew);
			} else if (c.matches(".*:tv_.*")) {
				WordEditorForm pane = new WordEditorForm("Transitive Verb", wew, this);
				pane.addRow("third singular", new TextField(), "", true);
				pane.addRow("bare infinitive", new TextField(), "", true);
				pane.addRow("past participle", new TextField(), "", false);
				wew.addTab(pane);
				editor.showWindow(wew);
			} else if (c.matches(".*:adj_itr.*")) {
				WordEditorForm pane = new WordEditorForm("Intransitive Adjective", wew, this);
				pane.addRow("positive", new TextField(), "", true);
				pane.addRow("comparative", new TextField(), "", false);
				pane.addRow("superlative", new TextField(), "", false);
				wew.addTab(pane);
				editor.showWindow(wew);
			} else if (c.matches(".*:adj_tr.*")) {
				WordEditorForm pane = new WordEditorForm("Transitive Adjective", wew, this);
				pane.addRow("positive", new TextField(), "", true);
				pane.addRow("comparative", new TextField(), "", false);
				wew.addTab(pane);
				editor.showWindow(wew);
			} else if (c.matches(".*:adv.*")) {
				WordEditorForm pane = new WordEditorForm("Adverb", wew, this);
				pane.addRow("positive", new TextField(), "", true);
				pane.addRow("comparative", new TextField(), "", false);
				pane.addRow("superlative", new TextField(), "", false);
				wew.addTab(pane);
				editor.showWindow(wew);
			} else if (c.matches(".*:prep:.*")) {
				WordEditorForm pane = new WordEditorForm("Preposition", wew, this);
				pane.addRow("preposition", new TextField(), "", true);
				wew.addTab(pane);
				editor.showWindow(wew);
			}
		}
		
		if (e.getSource() instanceof WordEditorWindow) {
			WordEditorWindow wew = (WordEditorWindow) e.getSource();
			if (e.getActionCommand().equals("OK")) {
				
				for (String s : wew.getCurrentTab().getRequiredTextFieldContents()) {
					if (s.matches("(\\s|_)*")) {
						editor.showWindow(new MessageWindow(
								"Error",
								"A required word form is missing.",
								wew,
								"OK"
							));
						return;
					}
				}
				
				List<String> w = new ArrayList<String>();
				for (Component comp : wew.getCurrentTab().getFormElements()) {
					if (!(comp instanceof TextField)) continue;
					String t = ((TextField) comp).getText().replaceAll("(\\s|_)+", "_")
							.replaceAll("(^_|_$)", "");
					
					if (FunctionWords.isFunctionWord(t)) {
						editor.showWindow(new MessageWindow(
								"Error",
								"This is a function word that cannot be redefined: '" + t + "'",
								wew,
								"OK"
							));
						return;
					} else if (t.length() > 0 && !t.matches("[a-zA-Z][a-zA-Z0-9_-]*")) {
						editor.showWindow(new MessageWindow(
								"Error",
								"This is not a valid word: '" + t + "'",
								wew,
								"OK"
							));
						return;
					} else if (!t.equals("") && w.contains(t)) {
						editor.showWindow(new MessageWindow(
								"Error",
								"All word forms must be distinct.",
								wew,
								"OK"
							));
						return;
					} else if (lexiconHandler.getWordsByText(t).size() > 0) {
						editor.showWindow(new MessageWindow(
								"Error",
								"This word is already used in the lexicon: '" + t + "'",
								wew,
								"OK"
							));
						return;
					}
					w.add(t);
				}
				
				Word word = null;
				c = wew.getType();
				if (c.matches(".*:prop_sg:.*")) {
					if (((CheckBox) wew.getCurrentTab().getFormElements().get(1)).isSelected()) {
						word = new Word("pndef_sg('" + w.get(0) + "','" + w.get(0) + "',undef).");
					} else {
						word = new Word("pn_sg('" + w.get(0) + "','" + w.get(0) + "',undef).");
					}
					lexiconHandler.addWord(word);
				} else if (c.matches(".*:noun_.*")) {
					Word wordsg = new Word("noun_sg('" + w.get(0) + "','" + w.get(0) + "',undef).");
					Word wordpl = new Word("noun_pl('" + w.get(1) + "','" + w.get(0) + "',undef).");
					if (c.matches(".*:noun_sg:.*")) {
						word = wordsg;
					} else {
						word = wordpl;
					}
					lexiconHandler.addWord(wordsg);
					lexiconHandler.addWord(wordpl);
				} else if (c.matches(".*:iv_.*")) {
					Word wordsg = new Word("iv_finsg('" + w.get(0) + "','" + w.get(1) + "').");
					Word wordpl = new Word("iv_infpl('" + w.get(1) + "','" + w.get(1) + "').");
					if (c.matches(".*:iv_finsg:.*")) {
						word = wordsg;
					} else {
						word = wordpl;
					}
					lexiconHandler.addWord(wordsg);
					lexiconHandler.addWord(wordpl);
				} else if (c.matches(".*:tv_.*")) {
					Word wordsg = new Word("tv_finsg('" + w.get(0) + "','" + w.get(1) + "').");
					Word wordpl = new Word("tv_infpl('" + w.get(1) + "','" + w.get(1) + "').");
					Word wordpp = null;
					if (!w.get(2).equals("")) {
						wordpp = new Word("tv_pp('" + w.get(2) + "','" + w.get(1) + "').");
					}
					if (c.matches(".*:tv_finsg:.*")) {
						word = wordsg;
					} else if (c.matches(".*:tv_infpl:.*")) {
						word = wordpl;
					} else {
						word = wordpp;
					}
					if (word == null) {
						editor.showWindow(new MessageWindow(
								"Error",
								"The word form to be used is missing.",
								wew,
								"OK"
							));
						return;
					}
					lexiconHandler.addWord(wordsg);
					lexiconHandler.addWord(wordpl);
					lexiconHandler.addWord(wordpp);
				} else if (c.matches(".*:adj_itr.*")) {
					word = new Word("adj_itr('" + w.get(0) + "','" + w.get(0) + "').");
					lexiconHandler.addWord(word);
					if (!w.get(1).equals("")) {
						lexiconHandler.addWord("adj_itr_comp('" + w.get(1) + "','" + w.get(0) + "').");
					}
					if (!w.get(2).equals("")) {
						lexiconHandler.addWord("adj_itr_sup('" + w.get(2) + "','" + w.get(0) + "').");
					}
				} else if (c.matches(".*:adj_tr.*")) {
					String prep = w.get(0).replaceFirst("^.*[_-]([a-zA-Z0-9]+)$", "$1");
					word = new Word("adj_tr('" + w.get(0) + "','" + w.get(0) + "', '" + prep + "').");
					if (prep.equals(w.get(0))) {
						editor.showWindow(new MessageWindow(
								"Error",
								"Transitive adjectives must end with a preposition (e.g. '...-for').",
								wew,
								"OK"
							));
						return;
					}
					if (!w.get(1).equals("")) {
						String prepc = w.get(1).replaceFirst("^.*[_-]([a-zA-Z0-9]+)$", "$1");
						if (prepc.equals(w.get(1))) {
							editor.showWindow(new MessageWindow(
									"Error",
									"Transitive adjectives must end with a preposition (e.g. '...-for').",
									wew,
									"OK"
								));
							return;
						}
						if (!prep.equals(prepc)) {
							editor.showWindow(new MessageWindow(
									"Error",
									"Positive and comparative form must end with the same preposition.",
									wew,
									"OK"
								));
							return;
						}
					}
					lexiconHandler.addWord(word);
					if (!w.get(1).equals("")) {
						lexiconHandler.addWord("adj_tr_comp('" + w.get(1) + "','" + w.get(0) + "', '" + prep + "').");
					}
				} else if (c.matches(".*:adv.*")) {
					word = new Word("adv('" + w.get(0) + "','" + w.get(0) + "').");
					lexiconHandler.addWord(word);
					if (!w.get(1).equals("")) {
						lexiconHandler.addWord("adv_comp('" + w.get(1) + "','" + w.get(0) + "').");
					}
					if (!w.get(2).equals("")) {
						lexiconHandler.addWord("adv_sup('" + w.get(2) + "','" + w.get(0) + "').");
					}
				} else if (c.matches(".*:prep:.*")) {
					word = new Word("prep('" + w.get(0) + "','" + w.get(0) + "').");
					lexiconHandler.addWord(word);
				}
				
				if (word != null) {
					preditorWindow.addTextElement(word.getTextElement());
				}
			}
			editor.removeWindow(wew);
		}
	}

}

