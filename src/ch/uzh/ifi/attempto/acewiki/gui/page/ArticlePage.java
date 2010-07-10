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

import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Comment;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Concept;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Individual;
import ch.uzh.ifi.attempto.acewiki.core.ontology.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Role;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Statement;
import ch.uzh.ifi.attempto.acewiki.gui.CommentRow;
import ch.uzh.ifi.attempto.acewiki.gui.DropDownMenu;
import ch.uzh.ifi.attempto.acewiki.gui.TextRow;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.acewiki.gui.editor.CommentEditorHandler;
import ch.uzh.ifi.attempto.acewiki.gui.editor.SentenceEditorHandler;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;

/**
 * This class stands for a wiki page that represents an ontology element and shows the
 * article of this ontology element.
 * 
 * @author Tobias Kuhn
 */
public abstract class ArticlePage extends WikiPage implements ActionListener {
	
	private static final long serialVersionUID = -297830105047433502L;
	
	private Column textColumn = new Column();
	private DropDownMenu dropDown = new DropDownMenu(DropDownMenu.EMPTY_TYPE, this);
	
	/**
	 * Creates a new article page.
	 * 
	 * @param wiki The wiki instance.
	 * @param ontologyElement The ontology element whose article should be shown.
	 */
	protected ArticlePage(Wiki wiki, OntologyElement ontologyElement) {
		super(wiki, new Title(ontologyElement.getHeadword()));
		
		addSelectedTab("Article");
		addTab(ontologyElement.getType(), "Word", this);
		addTab("References", this);
		
		dropDown.addMenuEntry("Add Sentence...", "Add a new sentence here");
		dropDown.addMenuEntry("Add Comment...", "Add a new comment here");
		
		textColumn.setInsets(new Insets(10, 20, 0, 70));
		textColumn.setCellSpacing(new Extent(2));
		add(textColumn);
	}
	
	/**
	 * Creates an article page for the given ontology element.
	 * 
	 * @param oe The ontology element for which an article page should be created.
	 * @param wiki The wiki instance.
	 * @return The new article page.
	 */
	public static ArticlePage create(OntologyElement oe, Wiki wiki) {
		if (oe instanceof Individual) {
			return new IndividualPage((Individual) oe, wiki);
		} else if (oe instanceof Concept) {
			return new ConceptPage((Concept) oe, wiki);
		} else if (oe instanceof Role) {
			return new RolePage((Role) oe, wiki);
		}
		return null;
	}
	
	/**
	 * Returns the ontology element of this article page.
	 * 
	 * @return The ontology element.
	 */
	public abstract OntologyElement getOntologyElement();
	
	protected void doUpdate() {
		textColumn.removeAll();
		
		for (Statement s : getOntologyElement().getStatements()) {
			if (s instanceof Sentence) {
				textColumn.add(new TextRow((Sentence) s, this));
			} else if (s instanceof Comment) {
				textColumn.add(new CommentRow((Comment) s, this));
			}
		}
		
		if (getOntologyElement().getStatements().size() == 0) {
			textColumn.add(new SolidLabel("(article is empty)", Font.ITALIC, 10));
		}
		
		if (!getWiki().isReadOnly()) {
			Row addButtonRow = new Row();
			addButtonRow.add(dropDown);
			textColumn.add(addButtonRow);
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof ArticlePage) {
			return getOntologyElement() == ((ArticlePage) obj).getOntologyElement();
		}
		return false;
	}
	
	public String toString() {
		return getOntologyElement().getWord();
	}
	
	public boolean isExpired() {
		return !getWiki().getOntology().contains(getOntologyElement());
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("Add Sentence...")) {
			getWiki().log("page", "dropdown: add sentence");
			getWiki().showWindow(SentenceEditorHandler.generateCreationWindow(null, this));
		} else if (e.getActionCommand().equals("Add Comment...")) {
			getWiki().log("page", "dropdown: add comment");
			getWiki().showWindow(CommentEditorHandler.generateCreationWindow(null, this));
		} else if ("References".equals(e.getActionCommand())) {
			log("page", "pressed: references");
			getWiki().showPage(new ReferencesPage(this));
		} else if ("Word".equals(e.getActionCommand())) {
			log("page", "pressed: word");
			getWiki().showPage(new WordPage(this));
		}
	}

}
