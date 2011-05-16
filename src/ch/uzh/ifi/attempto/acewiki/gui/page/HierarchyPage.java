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

package ch.uzh.ifi.attempto.acewiki.gui.page;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ReasonerManager;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.gui.IndexBar;
import ch.uzh.ifi.attempto.acewiki.gui.RecalcIcon;
import ch.uzh.ifi.attempto.acewiki.gui.SentenceComponent;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a page that shows the super-concepts and sub-concepts for a given concept.
 * Such super- and sub-concept relations are called "hierarchy" in AceWiki.
 * 
 * @author Tobias Kuhn
 */
public class HierarchyPage extends WikiPage implements ActionListener {

	private static final long serialVersionUID = 3126817139010810197L;

	private static final int pageSize = 25;
	
	private ConceptPage page;
	private RecalcIcon upRecalcIcon = new RecalcIcon("This list is being updated.");
	private RecalcIcon downRecalcIcon = new RecalcIcon("This list is being updated.");
	private Title title;

	private Column upHierarchyColumn = new Column();
	private Column downHierarchyColumn = new Column();
	private int upChosenPage = 0;
	private int downChosenPage = 0;
	
	/**
	 * Creates a new hierarchy page.
	 * 
	 * @param page The main page that contains the article.
	 */
	public HierarchyPage(ConceptPage page) {
		super(page.getWiki());
		this.page = page;
		
		addTab("Article", this);
		addTab("References", this);
		addTab("Individuals", this);
		addSelectedTab("Hierarchy");
		
		OntologyElement oe = page.getOntologyElement();
		title = new Title(oe.getHeadword(), "- Hierarchy", oe.getType(), this);
		add(title);
		addHorizontalLine();
		add(new VSpace(12));
		
		upRecalcIcon.setVisible(false);
		addHeadline("Upward", upRecalcIcon);
		add(new VSpace(5));
		add(upHierarchyColumn);
		
		downRecalcIcon.setVisible(false);
		addHeadline("Downward", downRecalcIcon);
		add(new VSpace(5));
		add(downHierarchyColumn);
	}
	
	protected void doUpdate() {
		title.setText(page.getOntologyElement().getHeadword());
		upHierarchyColumn.removeAll();
		downHierarchyColumn.removeAll();
		
		Concept c = (Concept) page.getOntologyElement();
		ReasonerManager rm = getWiki().getOntology().getReasonerManager();
		
		if (rm.areCachedSuperConceptsUpToDate(c)) {
			upHierarchyColumn.add(new HierarchyComponent(true, true));
		} else {
			upRecalcIcon.setVisible(true);
			upHierarchyColumn.add(new HierarchyComponent(true, true));
			page.getWiki().enqueueWeakAsyncTask(new Task() {
				
				private HierarchyComponent delayedComp;
				
				public void run() {
					delayedComp = new HierarchyComponent(true, false);
				}
				
				public void updateGUI() {
					upHierarchyColumn.removeAll();
					upHierarchyColumn.add(delayedComp);
					upRecalcIcon.setVisible(false);
				}
				
			});
		}

		if (rm.areCachedSuperConceptsUpToDate(c)) {
			downHierarchyColumn.add(new HierarchyComponent(false, true));
		} else {
			downRecalcIcon.setVisible(true);
			downHierarchyColumn.add(new HierarchyComponent(false, true));
			page.getWiki().enqueueWeakAsyncTask(new Task() {
				
				private HierarchyComponent delayedComp;
				
				public void run() {
					delayedComp = new HierarchyComponent(false, false);
				}
				
				public void updateGUI() {
					downHierarchyColumn.removeAll();
					downHierarchyColumn.add(delayedComp);
					downRecalcIcon.setVisible(false);
				}
				
			});
		}
	}

	public void actionPerformed(ActionEvent e) {
		if ("Article".equals(e.getActionCommand())) {
			log("page", "pressed: article");
			getWiki().showPage(page);
		} else if ("References".equals(e.getActionCommand())) {
			log("page", "pressed: references");
			getWiki().showPage(new ReferencesPage(page));
		} else if ("Individuals".equals(e.getActionCommand())) {
			log("page", "pressed: individuals");
			getWiki().showPage(new IndividualsPage(page));
		} else if (e.getSource() == title) {
			getWiki().showEditorWindow(page.getOntologyElement());
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof HierarchyPage) {
			return page.equals(((HierarchyPage) obj).page);
		}
		return false;
	}
	
	public boolean isExpired() {
		return page.isExpired();
	}
	
	public String toString() {
		return "-IND- " + page.getOntologyElement().getWord();
	}

	private class HierarchyComponent extends Column implements ActionListener {
		
		private static final long serialVersionUID = 6461817187189387351L;
		
		private boolean up;
		private Column column = new Column();
		private IndexBar indexBar;
		private List<Sentence> sentences;
		
		
		public HierarchyComponent(boolean up, boolean cached) {
			this.up = up;
			indexBar = new IndexBar("Page:", 0, this);
			add(indexBar);
			column.setInsets(new Insets(10, 2, 5, 10));
			column.setCellSpacing(new Extent(2));
			add(column);
			
			Concept concept = (Concept) page.getOntologyElement();
			ReasonerManager rm = getWiki().getOntology().getReasonerManager();
			List<Concept> concepts;
			
			if (up) {
				if (cached) {
					concepts = rm.getCachedSuperConcepts(concept);
				} else {
					concepts = rm.getSuperConcepts(concept);
				}
			} else {
				if (cached) {
					concepts = rm.getCachedSubConcepts(concept);
				} else {
					concepts = rm.getSubConcepts(concept);
				}
			}
			if (concepts != null) {
				sentences = new ArrayList<Sentence>();
				Collections.sort(concepts);
				for (Concept c : concepts) {
					if (up) {
						sentences.add(getWiki().getOntology().getStatementFactory().createSentence(
								"Every " + concept.getWord() + " is a " + c.getWord() + "."
							));
					} else {
						sentences.add(getWiki().getOntology().getStatementFactory().createSentence(
								"Every " + c.getWord() + " is a " + concept.getWord() + "."
							));
					}
				}
			}
			
			updatePage();
		}
		
		private void updatePage() {
			column.removeAll();
			
			String t;
			int chosenPage;
			if (up) {
				t = "upward";
				chosenPage = upChosenPage;
			} else {
				t = "downward";
				chosenPage = downChosenPage;
			}
			
			if (sentences == null) {
				column.add(new SolidLabel("...", Font.ITALIC, 10));
				indexBar.setVisible(false);
			} else {
				if (sentences.size() == 0) {
					indexBar.setVisible(false);
					column.add(new SolidLabel("(" + t + " hierarchy is empty)", Font.ITALIC, 10));
				} else {
					int i = ((sentences.size()-1) / pageSize) + 1;
					if (chosenPage > i) chosenPage = 0;
					indexBar.setNumbers(i);
					indexBar.setActiveButton(chosenPage);
				}
				
				indexBar.setVisible(sentences.size() > pageSize);
				
				int max = sentences.size();
				if (max > (chosenPage + 1) * pageSize) max = (chosenPage + 1) * pageSize;
				for (int i = chosenPage * pageSize; i < max; i++) {
					Row r = new Row();
					r.add(new SentenceComponent(sentences.get(i), HierarchyPage.this));
					column.add(r);
				}
			}
		}
		
		public void actionPerformed(ActionEvent e) {
			if (e.getSource() == indexBar) {
				if (up) {
					upChosenPage = Integer.parseInt(e.getActionCommand()) - 1;
					log("page", "pressed: page up:" + (upChosenPage+1));
					updatePage();
				} else {
					downChosenPage = Integer.parseInt(e.getActionCommand()) - 1;
					log("page", "pressed: page down:" + (downChosenPage+1));
					updatePage();
				}
			}
		}
		
	}

}
