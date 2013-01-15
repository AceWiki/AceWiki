// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
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

import nextapp.echo.app.ApplicationInstance;
import nextapp.echo.app.Color;
import nextapp.echo.app.event.ActionEvent;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.AceWikiReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.echocomp.EchoThread;

/**
 * This class stands for an article page showing the article of a concept. At the
 * moment, concepts are represented only by nouns.
 * 
 * @author Tobias Kuhn
 */
public class ConceptPage extends ArticlePage {
	
	private static final long serialVersionUID = -505381176379658743L;

	private Concept concept;
	
	/**
	 * Creates a new article page for a concept.
	 * 
	 * @param concept The concept.
	 * @param wiki The wiki instance.
	 */
	protected ConceptPage(Concept concept, Wiki wiki) {
		super(wiki, concept);
		this.concept = concept;
		
		addTab("acewiki_page_individuals", this);
		addTab("acewiki_page_hierarchy", this);
	}
	
	public OntologyElement getOntologyElement() {
		return concept;
	}

	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		if ("acewiki_page_individuals".equals(e.getActionCommand())) {
			getWiki().showPage(new IndividualsPage(this));
		} else if ("acewiki_page_hierarchy".equals(e.getActionCommand())) {
			getWiki().showPage(new HierarchyPage(this));
		}
	}
	
	protected void doUpdate() {
		super.doUpdate();
		
		getTitle().setText(getHeading(concept));
		
		EchoThread thread = new EchoThread() {
			
			public ApplicationInstance getApplication() {
				return getWiki().getApplication();
			}
			
			public void run() {
				synchronized (getWiki().getApplication()) {
					getWiki().enqueueWeakAsyncTask(new Task() {
						
						private boolean satisfiable;
						
						public void run() {
							AceWikiReasoner r = concept.getOntology().getReasoner();
							satisfiable = r.isSatisfiable(concept);
						}
						
						public void updateGUI() {
							if (satisfiable) {
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
