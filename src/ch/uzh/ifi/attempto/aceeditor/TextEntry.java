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

package ch.uzh.ifi.attempto.aceeditor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Button;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.ResourceImageReference;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.layout.RowLayoutData;
import ch.uzh.ifi.attempto.ape.ACEParser;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.APELocal;
import ch.uzh.ifi.attempto.ape.Lexicon;
import ch.uzh.ifi.attempto.ape.Message;
import ch.uzh.ifi.attempto.ape.OutputType;
import ch.uzh.ifi.attempto.echocomp.SquareButton;
import ch.uzh.ifi.attempto.echocomp.Style;

/**
 * This class represents a text entry of the ACE Editor. Such a text entry consists of an ACE text
 * or a comment. In the case it is an ACE text, the text entry also contains result items that show
 * the parsing results. These parsing results can be hidden (collapsed) or shown (expanded).
 * Comments start with "# ".
 * 
 * @author Tobias Kuhn
 */
class TextEntry extends Column implements ActionListener {

	private static final long serialVersionUID = -1976178070379593477L;
	
	private static final String imgpath = "ch/uzh/ifi/attempto/echocomp/style/";

	// Use a local instance of APE:
	private static ACEParser aceParser = APELocal.getInstance();
	// The web service could be used instead:
	//private static ACEParser aceParser = new APEWebservice("http://attempto.ifi.uzh.ch/ws/ape/apews.perl");

	private Map<String,ResultItem> resultItems = new HashMap<String,ResultItem>();

	private boolean selected = false;
	private boolean expanded = true;
	private boolean error = false;
	private boolean comment = false;
	private ACEEditor owner;
	private String text;

	private Column resultsColumn;
	private Button sentenceButton;
	private SquareButton expandButton = new SquareButton("right", this);

	/**
	 * Creates a new text entry with collapsed result items.
	 * 
	 * @param text The ACE text or comment.
	 * @param owner The ACE Editor object.
	 */
	public TextEntry(String text, ACEEditor owner) {
		this(text, owner, null, false);
	}

	/**
	 * Creates a new text entry.
	 * 
	 * @param text The ACE text or comment.
	 * @param owner The ACE Editor object.
	 * @param expanded true if the result items should be expanded, false if they should be
	 *     collapsed.
	 */
	public TextEntry(String text, ACEEditor owner, boolean expanded) {
		this(text, owner, null, expanded);
	}

	/**
	 * Creates a new text entry with the results items according to the given template.
	 * 
	 * @param text The ACE text or comment.
	 * @param owner The ACE Editor object.
	 * @param resultItemsTemplate The template according to which the result items are generated.
	 * @param expanded true if the result items should be expanded, false if they should be
	 *     collapsed.
	 */
	private TextEntry(String text, ACEEditor owner, Map<String,ResultItem> resultItemsTemplate,
			boolean expanded) {
		this.owner = owner;
		this.expanded = expanded;
		if (text == null || text.matches("\\s*")) {
			this.text = null;
		} else {
			this.text = text.replaceAll("\\s+", " ");
		}

		Row sentenceRow = new Row();
		sentenceRow.setInsets(new Insets(0, 0, 5, 0));
		RowLayoutData layout = new RowLayoutData();
		layout.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		
		expandButton.setLayoutData(layout);
		sentenceRow.add(expandButton);

		sentenceButton = new Button();
		sentenceButton.setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(13)));
		sentenceButton.setInsets(new Insets(4, 1, 4, 2));
		sentenceButton.setRolloverEnabled(true);
		sentenceButton.addActionListener(this);
		sentenceButton.setLayoutData(layout);
		sentenceRow.add(sentenceButton);

		add(sentenceRow);

		resultsColumn = new Column();
		resultsColumn.setInsets(new Insets(25, 5, 5, 5));
		if (text != null) {
			List<ResultItem> l = new ArrayList<ResultItem>();

			l.add(createResultItem(ResultItem.PARAPHRASE, resultItemsTemplate));
			l.add(createResultItem(ResultItem.SENTENCES, resultItemsTemplate));
			l.add(createResultItem(ResultItem.TOKENS, resultItemsTemplate));
			l.add(createResultItem(ResultItem.SYNTAX_LIST, resultItemsTemplate));
			l.add(createResultItem(ResultItem.SYNTAX_TREE, resultItemsTemplate));
			l.add(createResultItem(ResultItem.SYNTAX_BOXES, resultItemsTemplate));
			l.add(createResultItem(ResultItem.DRS_PROLOG, resultItemsTemplate));
			l.add(createResultItem(ResultItem.DRS_PRETTY, resultItemsTemplate));
			l.add(createResultItem(ResultItem.DRS_XML, resultItemsTemplate));
			l.add(createResultItem(ResultItem.FOL, resultItemsTemplate));
			l.add(createResultItem(ResultItem.PNF, resultItemsTemplate));
			l.add(createResultItem(ResultItem.OWL_FSS, resultItemsTemplate));
			l.add(createResultItem(ResultItem.OWL_XML, resultItemsTemplate));

			for (ResultItem ri : l) {
				resultItems.put(ri.getType(), ri);
				resultsColumn.add(ri);
			}

			if (expanded) {
				add(resultsColumn);
			}
		}

		update();
	}

	/**
	 * Returns the ACE text or comment (starting with "# ") of this text entry.
	 * 
	 * @return The ACE text or comment.
	 */
	public String getText() {
		return text;
	}

	/**
	 * Sets the ACE text or comment (starting with "# ") of this text entry.
	 * 
	 * @param text The new ACE text or comment.
	 */
	public void setText(String text) {
		if (text == null || text.matches("\\s*")) {
			this.text = null;
		} else {
			this.text = text.replaceAll("\\s+", " ");
		}
		update();
	}

	/**
	 * Sets the selection status of this text entry.
	 * 
	 * @param selected true for marking this text entry as selected, false to mark it as
	 *     unselected.
	 */
	public void setSelected(boolean selected) {
		this.selected = selected;
		if (comment) {
			sentenceButton.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		} else {
			sentenceButton.setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(13)));
		}
		if (selected) {
			sentenceButton.setBackground(Style.mediumBackground);
			sentenceButton.setRolloverBackground(Style.mediumBackground);
			if (error) {
				sentenceButton.setForeground(Color.RED);
				sentenceButton.setRolloverForeground(Color.RED);
			} else if (comment) {
				sentenceButton.setForeground(new Color(240, 240, 240));
				sentenceButton.setRolloverForeground(new Color(240, 240, 240));
			} else {
				sentenceButton.setForeground(Color.WHITE);
				sentenceButton.setRolloverForeground(Color.WHITE);
			}
		} else {
			sentenceButton.setBackground(Color.WHITE);
			sentenceButton.setRolloverBackground(Style.lightBackground);
			if (error) {
				sentenceButton.setForeground(Color.RED);
				sentenceButton.setRolloverForeground(Color.RED);
			} else if (comment) {
				sentenceButton.setForeground(Color.DARKGRAY);
				sentenceButton.setRolloverForeground(Color.DARKGRAY);
			} else {
				sentenceButton.setForeground(Color.BLACK);
				sentenceButton.setRolloverForeground(Color.BLACK);
			}
		}
	}

	/**
	 * Sets the visibility status of a certain result item of this text entry.
	 * 
	 * @param resultItemType The type of the result item.
	 * @param visible true if the result item should be visible, or false if it should be hidden.
	 */
	public void setResultItemVisible(String resultItemType, boolean visible) {
		ResultItem ri = resultItems.get(resultItemType);
		if (ri != null) {
			ri.setVisible(visible);
		}
	}

	/**
	 * Returns true if the given result item is visible.
	 * 
	 * @param resultItemType The type of the result item.
	 * @return true if the given result item is visible.
	 */
	public boolean isResultItemVisible(String resultItemType) {
		return resultItems.get(resultItemType).isVisible();
	}

	/**
	 * Returns true if this text entry contains an empty text.
	 * 
	 * @return true if this text entry is empty.
	 */
	public boolean isEmpty() {
		return text == null;
	}

	/**
	 * Returns true if the result items of this text entry are expanded.
	 * 
	 * @return true if this text entry is expanded.
	 */
	public boolean isExpanded() {
		return expanded;
	}

	/**
	 * Returns true if the text of this text entry cannot be parsed by the ACE parser.
	 * 
	 * @return true if this text entry contains a malformed ACE text.
	 */
	public boolean hasError() {
		return error;
	}

	/**
	 * Returns true if this text entry contains a comment and not an ACE text.
	 * 
	 * @return true if this text entry contains a comment.
	 */
	public boolean isComment() {
		return comment;
	}

	/**
	 * Expands or collapses the result items of this text entry.
	 * 
	 * @param expanded true to expand the text entry, or false to collapse it.
	 */
	public void setExpanded(boolean expanded) {
		if (this.expanded == expanded || text == null || comment) {
			return;
		}
		this.expanded = expanded;
		if (expanded) {
			expandButton.setIcon(new ResourceImageReference(imgpath + "down.png"));
			add(resultsColumn);
		} else {
			expandButton.setIcon(new ResourceImageReference(imgpath + "right.png"));
			remove(resultsColumn);
		}
		owner.entryChanged(this);
	}

	/**
	 * Copies this text entry.
	 * 
	 * @return A copy of this text entry.
	 */
	public TextEntry copy() {
		TextEntry copy = new TextEntry(text, owner, resultItems, expanded);
		copy.selected = selected;
		return copy;
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == expandButton) {
			if (!comment) {
				if (text == null) {
					owner.actionPerformed(new ActionEvent(this, "Add..."));
				} else {
					if (expanded) {
						setExpanded(false);
					} else {
						setExpanded(true);
					}
				}
			}
			owner.select(this);
		} else if (e.getSource() == sentenceButton) {
			owner.select(this);
		}
	}

	private ResultItem createResultItem(String name, Map<String,ResultItem> template) {
		if (template == null) {
			return new ResultItem(name);
		} else {
			return template.get(name).copy();
		}
	}

	private void update() {
		if (text == null) {
			expandButton.setIconName("plus");
			sentenceButton.setHeight(new Extent(17));
		} else if (text.startsWith("# ")) {
			expandButton.setIconName("diamond");
			sentenceButton.setText(text.substring(2));
			comment = true;
		} else {
			if (isExpanded()) {
				expandButton.setIconName("down");
			} else {
				expandButton.setIconName("right");
			}
			sentenceButton.setText(text);

			aceParser.setURI("http://attempto.ifi.uzh.ch/aceeditor/default");
			Lexicon lexicon;
			if (owner.isParseWithClexEnabled()) {
				aceParser.setClexEnabled(true);
				lexicon = null;
			} else {
				aceParser.setClexEnabled(false);
				lexicon = owner.getLexiconHandler().getLexicon();
			}
			// TODO: submit only the part of the lexicon that is needed.
			ACEParserResult parserResult = aceParser.getMultiOutput(text, lexicon,
					OutputType.PARAPHRASE,
					OutputType.SENTENCES,
					OutputType.TOKENS,
					OutputType.SYNTAX,
					OutputType.SYNTAXPP,
					OutputType.DRS,
					OutputType.DRSPP,
					OutputType.DRSXML,
					OutputType.FOL,
					OutputType.PNF,
					OutputType.OWLFSSPP,
					OutputType.OWLRDF,
					OutputType.OWLXML
			);

			for (ResultItem ri : resultItems.values()) {
				ri.setContent(parserResult);
			}
			
			for (Message m : parserResult.getMessageContainer().getErrorMessages()) {
				if (!m.getType().equals("owl")) {
					System.err.println(m.toString());
				}
			}

			error = parserResult.get(OutputType.DRS).equals("drs([], [])");
		}

		setSelected(selected);
	}

}
