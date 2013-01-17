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

import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.Article;
import ch.uzh.ifi.attempto.acewiki.core.Comment;
import ch.uzh.ifi.attempto.acewiki.core.Concept;
import ch.uzh.ifi.attempto.acewiki.core.GeneralTopic;
import ch.uzh.ifi.attempto.acewiki.core.Individual;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Relation;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.Statement;
import ch.uzh.ifi.attempto.acewiki.gfservice.GfModulePage;
import ch.uzh.ifi.attempto.acewiki.gfservice.TypeGfModule;
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
	private StatementMenu dropDown = new StatementMenu(StatementMenu.EMPTY_TYPE, getWiki(), this);
	private Title title;

	/**
	 * Creates a new article page.
	 * 
	 * @param wiki The wiki instance.
	 * @param ontologyElement The ontology element whose article should be shown.
	 */
	protected ArticlePage(Wiki wiki, OntologyElement ontologyElement) {
		super(wiki);

		if (!(ontologyElement.getInternalType().equals(GeneralTopic.MAINPAGE_TYPE))) {
			addSelectedTab("acewiki_page_article");
			addTab("acewiki_page_references", this);
			title = new Title(getHeading(ontologyElement), ontologyElement.getType(), this);
		} else {
			title = new Title("", true, null, null);
		}

		add(title);
		addHorizontalLine();

		dropDown.addMenuEntry("acewiki_statementmenu_addsent", "acewiki_statementmenu_addsenttooltip");
		dropDown.addMenuEntry("acewiki_statementmenu_addcomm", "acewiki_statementmenu_addcommtooltip");

		textColumn.setInsets(new Insets(10, 20, 0, 50));
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
		} else if (oe instanceof Relation) {
			return new RelationPage((Relation) oe, wiki);
		} else if (oe instanceof GeneralTopic) {
			if (oe.getInternalType().equals(GeneralTopic.MAINPAGE_TYPE)) {
				return new StartPage(wiki);
			} else {
				return new GeneralPage(oe, wiki);
			}
		} else if (oe instanceof TypeGfModule) {
			return new GfModulePage(oe, wiki);
		}
		return null;
	}

	/**
	 * Returns the ontology element of this article page.
	 * 
	 * @return The ontology element.
	 */
	public abstract OntologyElement getOntologyElement();

	/**
	 * Returns the article object.
	 * 
	 * @return The article.
	 */
	public Article getArticle() {
		return getOntologyElement().getArticle();
	}

	protected void doUpdate() {
		textColumn.removeAll();

		for (Statement s : getArticle().getStatements()) {
			if (s instanceof Sentence) {
				textColumn.add(new SentenceComponent((Sentence) s, this));
			} else if (s instanceof Comment) {
				textColumn.add(new CommentComponent((Comment) s, this));
			}
		}

		if (getArticle().getStatements().size() == 0) {
			textColumn.add(new SolidLabel(getWiki().getGUIText("acewiki_article_empty"), Font.ITALIC, 10));
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

	/**
	 * Returns the title object of this page.
	 * 
	 * @return The title.
	 */
	protected Title getTitle() {
		return title;
	}

	protected Column getTextColumn() {
		return textColumn;
	}

	protected StatementMenu getDropDown() {
		return dropDown;
	}


	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("acewiki_statementmenu_addsent")) {
			getWiki().log("page", "dropdown: add sentence");
			if (!getWiki().isEditable()) {
				getWiki().showLoginWindow();
			} else {
				getWiki().showWindow(SentenceEditorHandler.generateCreationWindow(null, this));
			}
		} else if (e.getActionCommand().equals("acewiki_statementmenu_addcomm")) {
			getWiki().log("page", "dropdown: add comment");
			if (!getWiki().isEditable()) {
				getWiki().showLoginWindow();
			} else {
				getWiki().showWindow(CommentEditorHandler.generateCreationWindow(null, this));
			}
		} else if ("acewiki_page_references".equals(e.getActionCommand())) {
			log("page", "pressed: references");
			getWiki().showPage(new ReferencesPage(this));
		} else if (e.getSource() == title) {
			getWiki().showEditorWindow(getOntologyElement());
		}
	}

}
