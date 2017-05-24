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
import java.util.Collections;
import java.util.Comparator;
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
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.StatementFactory;
import ch.uzh.ifi.attempto.echocomp.LocaleResources;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a page that shows all individuals that belong to a certain concept.
 * 
 * @author Tobias Kuhn
 */
public class IndividualsPage extends WikiPage implements ActionListener {

	private static final long serialVersionUID = 4273564259160715684L;

	private static final int pageSize = 50;
	
	private Concept concept;
	private Column individualsColumn = new Column();
	private int chosenPage = 0;
	private Title title;
	
	/**
	 * Creates a new individuals page.
	 *
	 * @param concept The concept in the assignments
	 * @param wiki The wiki that contains the new page
	 */
	public IndividualsPage(Concept concept, Wiki wiki) {
		super(wiki);
		this.concept = concept;

		title = new Title("", "", "", this);
		add(title);
		addHorizontalLine();
		
		add(individualsColumn);
	}
	
	protected void doUpdate() {
		setTabRow(TabRow.getArticleTabRow(concept, TabRow.TAB_INDIVIDUALS, getWiki()));

		title.setText(getHeading(concept));
		title.setPostTitle("- " + Wiki.getGUIText("acewiki_page_individuals"));
		title.setTooltip(concept.getType());
		individualsColumn.removeAll();
		
		final Column waitComp = new Column();
		waitComp.setInsets(new Insets(10, 0, 0, 0));
		waitComp.add(new RecalcIcon(Wiki.getGUIText("acewiki_list_updating")));

		CachingReasoner cr = getWiki().getOntology().getReasoner();
		
		if (cr.areCachedIndividualsUpToDate(concept)) {
			individualsColumn.add(new VSpace(18));
			individualsColumn.add(new IndividualsComponent(true));
		} else {
			individualsColumn.add(new VSpace(4));
			individualsColumn.add(waitComp);
			individualsColumn.add(new IndividualsComponent(true));
			getWiki().enqueueWeakAsyncTask(new Task() {
				
				private IndividualsComponent delayedComp;
				
				public void run() {
					delayedComp = new IndividualsComponent(false);
				}
				
				public void updateGUI() {
					individualsColumn.removeAll();
					individualsColumn.add(new VSpace(18));
					individualsColumn.add(delayedComp);
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
		if (obj instanceof IndividualsPage) {
			return concept.equals(((IndividualsPage) obj).concept);
		}
		return false;
	}
	
	public boolean isExpired() {
		return !getWiki().getOntology().contains(concept);
	}
	
	public String toString() {
		return "-IND- " + concept.getWord();
	}
	
	
	private class IndividualsComponent extends Column implements ActionListener {
		
		private static final long serialVersionUID = -2897618204616741456L;
		
		private Column sentencesColumn = new Column();
		private IndexBar indexBar;
		private List<Sentence> sentences;
		
		
		public IndividualsComponent(boolean cached) {
			indexBar = new IndexBar(0, this);
			add(indexBar);
			
			sentencesColumn.setInsets(new Insets(10, 2, 5, 20));
			sentencesColumn.setCellSpacing(new Extent(2));
			add(sentencesColumn);

			CachingReasoner cr = getWiki().getOntology().getReasoner();
			List<Individual> individuals;
			
			if (cached) {
				individuals = cr.getCachedIndividuals(concept);
			} else {
				individuals = cr.getIndividuals(concept);
			}
			if (individuals != null) {
				sentences = new ArrayList<Sentence>();
				
				Comparator<Individual> comparator = new Comparator<Individual>() {
					public int compare(Individual o1, Individual o2) {
						return LocaleResources.getCollator().compare(o1.getWord(3), o2.getWord(3));
					}
				};
				
				Collections.sort(individuals, comparator);
				for (Individual ind : individuals) {
					StatementFactory sf = getWiki().getOntology().getStatementFactory();
					sentences.add(sf.createAssignmentSentence(ind, concept));
				}
				if (sentences.size() == 0) {
					indexBar.setVisible(false);
					sentencesColumn.add(new SolidLabel(Wiki.getGUIText("acewiki_list_empty"), Font.ITALIC, 10));
				} else {
					int i = ((sentences.size()-1) / pageSize) + 1;
					if (chosenPage > i) chosenPage = 0;
					indexBar.setNumbers(i);
					indexBar.setActiveButton(chosenPage);
					updatePage();
				}
			} else {
				indexBar.setVisible(false);
				sentencesColumn.add(new SolidLabel("...", Font.ITALIC, 10));
			}
		}
		
		private void updatePage() {
			sentencesColumn.removeAll();
			
			indexBar.setVisible(sentences.size() > pageSize);
			
			int max = sentences.size();
			if (max > (chosenPage + 1) * pageSize) max = (chosenPage + 1) * pageSize;
			
			for (int i = chosenPage * pageSize; i < max; i++) {
				Row r = new Row();
				r.add(new SentenceComponent(sentences.get(i), IndividualsPage.this));
				sentencesColumn.add(r);
			}
		}

		public void actionPerformed(ActionEvent e) {
			if (e.getSource() == indexBar) {
				chosenPage = Integer.parseInt(e.getActionCommand()) - 1;
				log("page", "pressed: page " + (chosenPage+1));
				updatePage();
			}
		}
		
	}

}
