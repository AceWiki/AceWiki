// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.CachingReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.LanguageUtils;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.StatementFactory;
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
	
	private Concept concept;
	private RecalcIcon upRecalcIcon, downRecalcIcon;
	private Title title;

	private Column upHierarchyColumn = new Column();
	private Column downHierarchyColumn = new Column();
	private int upChosenPage = 0;
	private int downChosenPage = 0;
	
	/**
	 * Creates a new hierarchy page.
	 *
	 * @param concept The concept in the super/sup relations
	 * @param wiki The wiki that contains the new page
	 */
	public HierarchyPage(Concept concept, Wiki wiki) {
		super(wiki);
		this.concept = concept;

		title = new Title("", "", "", this);
		add(title);
		addHorizontalLine();
		add(new VSpace(12));

		upRecalcIcon = new RecalcIcon(getWiki().getGUIText("acewiki_list_updating"));
		upRecalcIcon.setVisible(false);
		addHeadline("acewiki_hierarchy_upheading", upRecalcIcon);
		add(new VSpace(5));
		add(upHierarchyColumn);

		downRecalcIcon = new RecalcIcon(getWiki().getGUIText("acewiki_list_updating"));
		downRecalcIcon.setVisible(false);
		addHeadline("acewiki_hierarchy_downheading", downRecalcIcon);
		add(new VSpace(5));
		add(downHierarchyColumn);
	}
	
	protected void doUpdate() {
		setTabRow(TabRow.getArticleTabRow(concept, TabRow.TAB_HIERARCHY, getWiki()));

		title.setText(getHeading(concept));
		title.setPostTitle("- " + getWiki().getGUIText("acewiki_page_hierarchy"));
		title.setTooltip(concept.getType());
		upHierarchyColumn.removeAll();
		downHierarchyColumn.removeAll();

		CachingReasoner cr = getWiki().getOntology().getReasoner();
		
		if (cr.areCachedSuperConceptsUpToDate(concept)) {
			upHierarchyColumn.add(new HierarchyComponent(true, true));
		} else {
			upRecalcIcon.setVisible(true);
			upHierarchyColumn.add(new HierarchyComponent(true, true));
			getWiki().enqueueWeakAsyncTask(new Task() {
				
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

		if (cr.areCachedSuperConceptsUpToDate(concept)) {
			downHierarchyColumn.add(new HierarchyComponent(false, true));
		} else {
			downRecalcIcon.setVisible(true);
			downHierarchyColumn.add(new HierarchyComponent(false, true));
			getWiki().enqueueWeakAsyncTask(new Task() {
				
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
		if (e.getSource() == title) {
			getWiki().showEditorWindow(concept);
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof HierarchyPage) {
			return concept.equals(((HierarchyPage) obj).concept);
		}
		return false;
	}
	
	public boolean isExpired() {
		return !getWiki().getOntology().contains(concept);
	}
	
	public String toString() {
		return "-IND- " + concept.getWord();
	}

	private class HierarchyComponent extends Column implements ActionListener {
		
		private static final long serialVersionUID = 6461817187189387351L;
		
		private boolean up;
		private Column column = new Column();
		private IndexBar indexBar;
		private List<Sentence> sentences;
		
		
		public HierarchyComponent(boolean up, boolean cached) {
			this.up = up;
			indexBar = new IndexBar(0, this);
			add(indexBar);
			column.setInsets(new Insets(10, 2, 5, 10));
			column.setCellSpacing(new Extent(2));
			add(column);

			CachingReasoner cr = getWiki().getOntology().getReasoner();
			List<Concept> concepts;
			
			if (up) {
				if (cached) {
					concepts = cr.getCachedSuperConcepts(concept);
				} else {
					concepts = cr.getSuperConcepts(concept);
				}
			} else {
				if (cached) {
					concepts = cr.getCachedSubConcepts(concept);
				} else {
					concepts = cr.getSubConcepts(concept);
				}
			}
			if (concepts != null) {
				sentences = new ArrayList<Sentence>();
				LanguageUtils.sortOntologyElements(concepts);
				for (Concept c : concepts) {
					StatementFactory sf = getWiki().getOntology().getStatementFactory();
					if (up) {
						sentences.add(sf.createHierarchySentence(concept, c));
					} else {
						sentences.add(sf.createHierarchySentence(c, concept));
					}
				}
			}
			
			updatePage();
		}
		
		private void updatePage() {
			column.removeAll();
			
			String message;
			int chosenPage;
			if (up) {
				message = getWiki().getGUIText("acewiki_hierarchy_upempty");
				chosenPage = upChosenPage;
			} else {
				message = getWiki().getGUIText("acewiki_hierarchy_downempty");
				chosenPage = downChosenPage;
			}
			
			if (sentences == null) {
				column.add(new SolidLabel("...", Font.ITALIC, 10));
				indexBar.setVisible(false);
			} else {
				if (sentences.size() == 0) {
					indexBar.setVisible(false);
					column.add(new SolidLabel(message, Font.ITALIC, 10));
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
