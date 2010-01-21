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

package ch.uzh.ifi.attempto.acewiki.gui.page;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Concept;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Individual;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Sentence;
import ch.uzh.ifi.attempto.acewiki.gui.IndexBar;
import ch.uzh.ifi.attempto.acewiki.gui.RecalcIcon;
import ch.uzh.ifi.attempto.acewiki.gui.TextRow;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a page that shows all individuals that belong to a certain concept.
 * 
 * @author Tobias Kuhn
 */
public class IndividualsPage extends WikiPage implements ActionListener {

	private static final long serialVersionUID = 4273564259160715684L;

	private static final int pageSize = 20;
	
	private ConceptPage page;
	private Column individualsColumn = new Column();
	private int chosenPage = 0;
	
	/**
	 * Creates a new individuals page.
	 * 
	 * @param page The main page that contains the article.
	 */
	public IndividualsPage(ConceptPage page) {
		super(page.getWiki(), new Title(page.getOntologyElement().getHeadword(), "- Individuals"));
		this.page = page;
		
		addTab("Article", this);
		addTab("Noun", this);
		addTab("References", this);
		addSelectedTab("Individuals");
		addTab("Hierarchy", this);
		
		add(new VSpace(18));
		
		add(individualsColumn);
	}
	
	protected void doUpdate() {
		getTitle().setText(page.getOntologyElement().getHeadword());
		individualsColumn.removeAll();
		
		final Column waitComp = new Column();
		waitComp.setInsets(new Insets(10, 0, 0, 0));
		waitComp.add(new RecalcIcon("This list is being updated."));
		
		if (((Concept) page.getOntologyElement()).areIndividualsCached()) {
			individualsColumn.add(new IndividualsComponent(true));
		} else {
			individualsColumn.add(waitComp);
			individualsColumn.add(new IndividualsComponent(true));
			page.getWiki().enqueueWeakAsyncTask(new Task() {
				
				private IndividualsComponent delayedComp;
				
				public void run() {
					delayedComp = new IndividualsComponent(false);
				}
				
				public void updateGUI() {
					individualsColumn.removeAll();
					individualsColumn.add(delayedComp);
				}
				
			});
		}
	}

	public void actionPerformed(ActionEvent e) {
		if ("Article".equals(e.getActionCommand())) {
			log("page", "pressed: article");
			getWiki().showPage(page);
		} else if ("Noun".equals(e.getActionCommand())) {
			log("page", "pressed: word");
			getWiki().showPage(new WordPage(page));
		} else if ("References".equals(e.getActionCommand())) {
			log("page", "pressed: references");
			getWiki().showPage(new ReferencesPage(page));
		} else if ("Hierarchy".equals(e.getActionCommand())) {
			log("page", "pressed: hierarchy");
			getWiki().showPage(new HierarchyPage(page));
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof IndividualsPage) {
			return page.equals(((IndividualsPage) obj).page);
		}
		return false;
	}
	
	public boolean isExpired() {
		return page.isExpired();
	}
	
	public String toString() {
		return "-IND- " + page.getOntologyElement().getWord();
	}
	
	
	private class IndividualsComponent extends Column implements ActionListener {
		
		private static final long serialVersionUID = -2897618204616741456L;
		
		private Column sentencesColumn = new Column();
		private IndexBar indexBar;
		private List<Sentence> sentences;
		
		
		public IndividualsComponent(boolean cached) {
			indexBar = new IndexBar("Page:", 0, this);
			add(indexBar);
			
			sentencesColumn.setInsets(new Insets(10, 2, 5, 20));
			sentencesColumn.setCellSpacing(new Extent(2));
			add(sentencesColumn);
			
			Concept concept = (Concept) page.getOntologyElement();
			List<Individual> individuals;
			if (cached) {
				individuals = concept.getCachedIndividuals();
			} else {
				individuals = concept.getIndividuals();
			}
			if (individuals != null) {
				sentences = new ArrayList<Sentence>();
				
				Comparator<Individual> comparator = new Comparator<Individual>() {
					public int compare(Individual o1, Individual o2) {
						return o1.getWord(2).compareTo(o2.getWord(2));
					}
				};
				
				Collections.sort(individuals, comparator);
				for (Individual ind : individuals) {
					sentences.add(new Sentence(
							ind.getWord(2) + " is a " + concept.getWord() + ".",
							concept.getOntology()
						));
				}
				if (sentences.size() == 0) {
					indexBar.setVisible(false);
					sentencesColumn.add(new SolidLabel("(no individual found)", Font.ITALIC, 10));
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
				r.add(new TextRow(sentences.get(i), IndividualsPage.this));
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
