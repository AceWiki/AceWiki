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

import java.util.ArrayList;
import java.util.List;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.button.ButtonGroup;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.event.WindowPaneEvent;
import nextapp.echo.app.event.WindowPaneListener;
import nextapp.echo.app.layout.GridLayoutData;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.LanguageHandler;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.acewiki.core.StatementFactory;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.RadioButton;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import echopoint.ContainerEx;
import echopoint.able.Scrollable;

/**
 * This window shows different alternatives of a sentence and allows the user to choose from them.
 * 
 * @author Tobias Kuhn
 */
public class AlternativesWindow extends WindowPane implements ActionListener {

	private static final long serialVersionUID = 6519074999256404080L;

	private Sentence sentence;
	private String language;
	private Wiki wiki;
	private ButtonGroup buttonGroup = new ButtonGroup();
	private List<RadioButton> radioButtons = new ArrayList<>();

	/**
	 * Creates a new alternatives window for the given sentence.
	 * 
	 * @param sentence The sentence.
	 * @param language The language for which the alternatives should be shown.
	 * @param wiki The wiki object.
	 */
	public AlternativesWindow(Sentence sentence, String language, Wiki wiki) {
		this.sentence = sentence;
		this.language = language;
		this.wiki = wiki;
		
		setTitle(wiki.getGUIText("acewiki_alternativeswindow_title"));
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setModal(true);
		setWidth(new Extent(770));
		setHeight(new Extent(347));
		setResizable(false);
		setMovable(true);
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");

		addWindowPaneListener(new WindowPaneListener() {

			private static final long serialVersionUID = -3897741327122083261L;

			public void windowPaneClosing(WindowPaneEvent e) {
				actionPerformed(new ActionEvent(AlternativesWindow.this, "general_action_close"));
			}

		});

		GridLayoutData layout1 = new GridLayoutData();
		layout1.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));

		Grid grid = new Grid(1);
		grid.setInsets(new Insets(10, 10, 0, 0));
		grid.setColumnWidth(0, new Extent(750));
		grid.setRowHeight(0, new Extent(55));
		grid.setRowHeight(1, new Extent(200));

		Column messageColumn = new Column();
		messageColumn.setLayoutData(layout1);

		Label label = new Label(wiki.getGUIText("acewiki_alternativeswindow_message"));
		label.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		messageColumn.add(label);
		messageColumn.add(new VSpace());
		grid.add(messageColumn);

		ContainerEx alternativesContainer = new ContainerEx();
		alternativesContainer.setScrollBarPolicy(Scrollable.AUTO);
		alternativesContainer.setWidth(new Extent(730));
		alternativesContainer.setHeight(new Extent(180));
		Grid alternativesGrid = new Grid(2);
		alternativesGrid.setColumnWidth(0, new Extent(25));
		for (TextContainer tc : sentence.getTextContainer(language)) {
			RadioButton rb = new RadioButton(buttonGroup);
			rb.setLayoutData(layout1);
			radioButtons.add(rb);
			alternativesGrid.add(rb);
			Column c = new Column();
			c.setInsets(new Insets(0, 0, 5, 5));
			c.add(new Label(tc.getText()));
			alternativesGrid.add(c);
		}
		alternativesContainer.add(alternativesGrid);
		alternativesContainer.setLayoutData(layout1);
		grid.add(alternativesContainer);

		Row buttonBar = new Row();
		buttonBar.setCellSpacing(new Extent(10));
		buttonBar.add(new GeneralButton("acewiki_alternativeswindow_button", this, 100));
		buttonBar.add(new GeneralButton("general_action_close", this, 100));
		GridLayoutData layout2 = new GridLayoutData();
		layout2.setAlignment(new Alignment(Alignment.CENTER, Alignment.TOP));
		buttonBar.setLayoutData(layout2);
		grid.add(buttonBar);

		add(grid);
	}

	/**
	 * Creates a new alternatives window for the given sentence and the language of the wiki.
	 * 
	 * @param sentence The sentence.
	 * @param wiki The wiki object.
	 */
	public AlternativesWindow(Sentence sentence, Wiki wiki) {
		this(sentence, wiki.getLanguage(), wiki);
	}

	public void actionPerformed(ActionEvent e) {
		String c = e.getActionCommand();
		if ("general_action_close".equals(c)) {
			setVisible(false);
		} else if ("acewiki_alternativeswindow_button".equals(c)) {
			if (!wiki.isEditable()) {
				wiki.showLoginWindow();
				return;
			}
			RadioButton selected = null;
			for (RadioButton rb : radioButtons) {
				if (rb.isSelected()) {
					selected = rb;
					break;
				}
			}
			if (selected == null) {
				wiki.showWindow(new MessageWindow(
						"acewiki_error_title",
						"acewiki_error_choose",
						this,
						"general_action_ok"
						));
				return;
			}
			int i = radioButtons.indexOf(selected);

			// TODO There should be a cleaner way to do this:
			LanguageHandler lh = wiki.getEngine().getLanguageHandler(language);
			TextContainer tc = sentence.getTextContainer(language).get(i);
			StatementFactory sf = wiki.getOntology().getStatementFactory();
			Sentence newSentence = sf.extractSentences(lh, tc, null, sentence.getArticle()).get(0);
			sentence.getArticle().edit(sentence, newSentence);

			wiki.refresh();
			setVisible(false);
		}
	}

}
