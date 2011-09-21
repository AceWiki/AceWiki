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
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.CachingReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.StatementFactory;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a page that shows to which concepts a certain individual belongs.
 * Such concept memberships are called "assignments" in AceWiki.
 * 
 * @author Tobias Kuhn
 */
public class AssignmentsPage extends WikiPage implements ActionListener {

	private static final long serialVersionUID = -6955789540998283993L;

	private static final int pageSize = 50;
	
	private IndividualPage page;
	private Column assignmentsColumn = new Column();
	private int chosenPage = 0;
	private Title title;
	
	/**
	 * Creates a new assignments page.
	 * 
	 * @param page The main page that contains the article.
	 */
	public AssignmentsPage(IndividualPage page) {
		super(page.getWiki());
		this.page = page;
		
		addTab("Article", this);
		addTab("References", this);
		addSelectedTab("Assignments");
		
		OntologyElement oe = page.getOntologyElement();
		title = new Title(getHeading(oe), "- Assignments", oe.getType(), this);
		add(title);
		addHorizontalLine();
		add(assignmentsColumn);
	}
	
	protected void doUpdate() {
		title.setText(getHeading(page.getOntologyElement()));
		assignmentsColumn.removeAll();
		
		final Column waitComp = new Column();
		waitComp.setInsets(new Insets(10, 0, 0, 0));
		waitComp.add(new RecalcIcon("This list is being updated."));
		
		CachingReasoner cr = getWiki().getOntology().getReasoner();
		
		if (cr.areCachedConceptsUpToDate((Individual) page.getOntologyElement())) {
			assignmentsColumn.add(new VSpace(18));
			assignmentsColumn.add(new AssignmentsComponent(true));
		} else {
			assignmentsColumn.add(new VSpace(4));
			assignmentsColumn.add(waitComp);
			assignmentsColumn.add(new AssignmentsComponent(true));
			page.getWiki().enqueueWeakAsyncTask(new Task() {
				
				private AssignmentsComponent delayedComp;
				
				public void run() {
					delayedComp = new AssignmentsComponent(false);
				}
				
				public void updateGUI() {
					assignmentsColumn.removeAll();
					assignmentsColumn.add(new VSpace(18));
					assignmentsColumn.add(delayedComp);
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
		} else if (e.getSource() == title) {
			getWiki().showEditorWindow(page.getOntologyElement());
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof AssignmentsPage) {
			return page.equals(((AssignmentsPage) obj).page);
		}
		return false;
	}
	
	public boolean isExpired() {
		return page.isExpired();
	}
	
	public String toString() {
		return "-ASS- " + page.getOntologyElement().getWord();
	}
	
	
	private class AssignmentsComponent extends Column implements ActionListener {
		
		private static final long serialVersionUID = -441448088305771435L;
		
		private Column sentencesColumn = new Column();
		private IndexBar indexBar;
		private List<Sentence> sentences;
		
		public AssignmentsComponent(boolean cached) {
			indexBar = new IndexBar("Page:", 0, this);
			add(indexBar);
			
			sentencesColumn.setInsets(new Insets(10, 2, 5, 20));
			sentencesColumn.setCellSpacing(new Extent(2));
			add(sentencesColumn);

			CachingReasoner cr = getWiki().getOntology().getReasoner();
			Individual ind = (Individual) page.getOntologyElement();
			List<Concept> concepts;
			if (cached) {
				concepts = cr.getCachedConcepts(ind);
			} else {
				concepts = cr.getConcepts(ind);
			}
			if (concepts != null) {
				sentences = new ArrayList<Sentence>();
				Collections.sort(concepts);
				for (Concept c : concepts) {
					StatementFactory sf = getWiki().getOntology().getStatementFactory();
					sentences.add(sf.createAssignmentSentence(ind, c));
				}
				if (sentences.size() == 0) {
					indexBar.setVisible(false);
					sentencesColumn.add(new SolidLabel("(no assignment found)", Font.ITALIC, 10));
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
				r.add(new SentenceComponent(sentences.get(i), AssignmentsPage.this));
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
