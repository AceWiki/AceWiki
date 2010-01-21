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

import nextapp.echo2.app.Color;
import nextapp.echo2.app.event.ActionEvent;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Concept;
import ch.uzh.ifi.attempto.acewiki.core.ontology.NounConcept;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;

/**
 * This class stands for an article page showing the article of a concept. At the
 * moment, concepts are represented only by nouns.
 * 
 * @author Tobias Kuhn
 */
public class ConceptPage extends ArticlePage {
	
	private static final long serialVersionUID = -505381176379658743L;

	private NounConcept concept;
	
	/**
	 * Creates a new article page for a concept.
	 * 
	 * @param concept The concept.
	 * @param wiki The wiki instance.
	 */
	protected ConceptPage(Concept concept, Wiki wiki) {
		super(wiki, concept);
		this.concept = (NounConcept) concept;
		
		addTab("Individuals", this);
		addTab("Hierarchy", this);
	}
	
	public OntologyElement getOntologyElement() {
		return concept;
	}

	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		if ("Individuals".equals(e.getActionCommand())) {
			getWiki().showPage(new IndividualsPage(this));
		} else if ("Hierarchy".equals(e.getActionCommand())) {
			getWiki().showPage(new HierarchyPage(this));
		}
	}
	
	protected void doUpdate() {
		super.doUpdate();
		
		getTitle().setText(concept.getHeadword());
		
		Thread thread = new Thread() {
			public void run() {
				synchronized (getWiki().getApplication()) {
					getWiki().enqueueTask(new Runnable() {
						public void run() {
							if (concept.getOntology().isSatisfiable(concept)) {
								getTitle().setColor(Color.BLACK);
							} else {
								getTitle().setColor(new Color(193, 0, 0));
							}
						}
					});
					try {
						sleep(500);
					} catch (InterruptedException ex) {}
				}
			}
		};
		thread.setPriority(Thread.MIN_PRIORITY);
		thread.start();
	}

}
