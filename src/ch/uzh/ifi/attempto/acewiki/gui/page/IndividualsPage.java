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
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ReasonerManager;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.StatementFactory;
import ch.uzh.ifi.attempto.acewiki.gui.IndexBar;
import ch.uzh.ifi.attempto.acewiki.gui.RecalcIcon;
import ch.uzh.ifi.attempto.acewiki.gui.SentenceComponent;
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

	private static final int pageSize = 50;
	
	private ConceptPage page;
	private Column individualsColumn = new Column();
	private int chosenPage = 0;
	private Title title;
	
	/**
	 * Creates a new individuals page.
	 * 
	 * @param page The main page that contains the article.
	 */
	public IndividualsPage(ConceptPage page) {
		super(page.getWiki());
		this.page = page;
		
		addTab("Article", this);
		addTab("References", this);
		addSelectedTab("Individuals");
		addTab("Hierarchy", this);
		
		OntologyElement oe = page.getOntologyElement();
		title = new Title(oe.getHeadword(), "- Individuals", oe.getType(), this);
		add(title);
		addHorizontalLine();
		add(new VSpace(18));
		
		add(individualsColumn);
	}
	
	protected void doUpdate() {
		title.setText(page.getOntologyElement().getHeadword());
		individualsColumn.removeAll();
		
		final Column waitComp = new Column();
		waitComp.setInsets(new Insets(10, 0, 0, 0));
		waitComp.add(new RecalcIcon("This list is being updated."));

		ReasonerManager rm = getWiki().getOntology().getReasonerManager();
		
		if (rm.areCachedIndividualsUpToDate((Concept) page.getOntologyElement())) {
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
		} else if ("References".equals(e.getActionCommand())) {
			log("page", "pressed: references");
			getWiki().showPage(new ReferencesPage(page));
		} else if ("Hierarchy".equals(e.getActionCommand())) {
			log("page", "pressed: hierarchy");
			getWiki().showPage(new HierarchyPage(page));
		} else if (e.getSource() == title) {
			getWiki().showEditorWindow(page.getOntologyElement());
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
			ReasonerManager rm = getWiki().getOntology().getReasonerManager();
			List<Individual> individuals;
			
			if (cached) {
				individuals = rm.getCachedIndividuals(concept);
			} else {
				individuals = rm.getIndividuals(concept);
			}
			if (individuals != null) {
				sentences = new ArrayList<Sentence>();
				
				Comparator<Individual> comparator = new Comparator<Individual>() {
					public int compare(Individual o1, Individual o2) {
						return o1.getWord(3).compareToIgnoreCase(o2.getWord(3));
					}
				};
				
				Collections.sort(individuals, comparator);
				for (Individual ind : individuals) {
					StatementFactory sf = getWiki().getOntology().getStatementFactory();
					sentences.add(sf.createAssignmentSentence(ind, concept));
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
