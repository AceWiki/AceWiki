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

package ch.uzh.ifi.attempto.aceeditor;

import java.util.Arrays;

import nextapp.echo.app.Border;
import nextapp.echo.app.Color;
import nextapp.echo.app.Column;
import nextapp.echo.app.Component;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import ch.uzh.ifi.attempto.ape.ACEParserResult;
import ch.uzh.ifi.attempto.ape.OutputType;
import ch.uzh.ifi.attempto.ape.SyntaxBoxes;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import echopoint.DirectHtml;
import echopoint.util.HtmlKit;

/**
 * This class represents a certain result of the parser for a certain ACE text. Such results can be
 * DRSs, paraphrases, syntax trees, etc.
 * 
 * @author Tobias Kuhn
 */
class ResultItem extends Row {

	private static final long serialVersionUID = -5108657342043962969L;

	// The possible types:
	public static final String PARAPHRASE = "Paraphrase";
	public static final String SENTENCES = "Sentences";
	public static final String TOKENS = "Tokens";
	public static final String SYNTAX_LIST = "Syntax List";
	public static final String SYNTAX_TREE = "Syntax Tree";
	public static final String SYNTAX_BOXES = "Syntax Boxes";
	public static final String DRS_PROLOG = "Prolog DRS";
	public static final String DRS_PRETTY = "Pretty-Printed DRS";
	public static final String DRS_XML = "XML DRS";
	public static final String FOL = "First-order Logic";
	public static final String PNF = "Normalized First-order Logic";
	public static final String OWL_FSS = "OWL FSS";
	public static final String OWL_XML = "OWL XML";

	public static final String[] TYPES = new String[] {
		PARAPHRASE, SENTENCES, TOKENS, SYNTAX_LIST, SYNTAX_TREE, SYNTAX_BOXES, DRS_PROLOG,
		DRS_PRETTY, DRS_XML, FOL, PNF, OWL_FSS, OWL_XML
	};

	private static final String[] PREFORMATED_TYPES = new String[] {
		SYNTAX_TREE, DRS_PRETTY, DRS_XML, OWL_FSS, OWL_XML
	};

	private static final String[] HTML_TYPES = new String[] {SYNTAX_BOXES};

	private String type;
	private String content;

	private Column contentColumn;

	/**
	 * Creates a new result item for the given type.
	 * 
	 * @param type The type of the result item to be created.
	 */
	public ResultItem(String type) {
		this.type = type;

		setInsets(new Insets(0, 0, 0, 5));

		Column mainColumn = new Column();
		mainColumn.setBorder(new Border(1, Color.BLACK, Border.STYLE_SOLID));

		Row headRow = new Row();
		headRow.setCellSpacing(new Extent(5));
		headRow.setInsets(new Insets(2, 0));
		headRow.setBackground(new Color(200, 200, 200));
		SolidLabel titleLabel = new SolidLabel(type, Font.PLAIN, 9);
		titleLabel.setForeground(Color.WHITE);

		headRow.add(titleLabel);
		contentColumn = new Column();
		contentColumn.setInsets(new Insets(10, 0));

		mainColumn.add(headRow);
		mainColumn.add(contentColumn);

		add(mainColumn);
		add(new HSpace());
	}

	/**
	 * Sets the content of this result item on the basis of the given parser result.
	 * 
	 * @param parserResult The parser result according to which this result item should be set.
	 */
	public void setContent(ACEParserResult parserResult) {
		setContent(getOutput(parserResult));
	}

	private void setContent(String content) {
		this.content = content;

		contentColumn.removeAll();
		Component contentComp;
		if (isPreformated()) {
			contentComp = new DirectHtml(
					"<pre style=\"font-size:11px;\">" + HtmlKit.encode(content) + "</pre>"
				);
		} else if (isHtml()) {
			contentComp = new DirectHtml("<span style=\"font-size:11px;\">" + content + "</span>");
		} else {
			Label l = new Label(content);
			l.setFont(new Font(Font.MONOSPACE, Font.PLAIN, new Extent(11)));
			Column c = new Column();
			c.setInsets(new Insets(0, 10));
			c.add(l);
			contentComp = c;
		}
		contentColumn.add(contentComp);
	}

	/**
	 * Returns the type of this result item.
	 * 
	 * @return The type of this result item.
	 */
	public String getType() {
		return type;
	}

	/**
	 * Copies this result item.
	 * 
	 * @return A copy of this result item.
	 */
	public ResultItem copy() {
		ResultItem copy = new ResultItem(type);
		copy.setContent(content);
		copy.setVisible(isVisible());
		return copy;
	}

	private String getOutput(ACEParserResult parserResult) {
		if (type.equals(ResultItem.PARAPHRASE)) return parserResult.get(OutputType.PARAPHRASE);
		if (type.equals(ResultItem.SENTENCES)) return parserResult.get(OutputType.SENTENCES);
		if (type.equals(ResultItem.TOKENS)) return parserResult.get(OutputType.TOKENS);
		if (type.equals(ResultItem.SYNTAX_LIST)) return parserResult.get(OutputType.SYNTAX);
		if (type.equals(ResultItem.SYNTAX_TREE)) return parserResult.get(OutputType.SYNTAXPP);
		if (type.equals(ResultItem.SYNTAX_BOXES)) return SyntaxBoxes.getBoxesHtml(parserResult);
		if (type.equals(ResultItem.DRS_PROLOG)) return parserResult.get(OutputType.DRS);
		if (type.equals(ResultItem.DRS_PRETTY)) return parserResult.get(OutputType.DRSPP);
		if (type.equals(ResultItem.DRS_XML)) return parserResult.get(OutputType.DRSXML);
		if (type.equals(ResultItem.FOL)) return parserResult.get(OutputType.FOL);
		if (type.equals(ResultItem.PNF)) return parserResult.get(OutputType.PNF);
		if (type.equals(ResultItem.OWL_FSS)) return parserResult.get(OutputType.OWLFSSPP);
		if (type.equals(ResultItem.OWL_XML)) return parserResult.get(OutputType.OWLXML);
		return "";
	}

	private boolean isPreformated() {
		return Arrays.asList(PREFORMATED_TYPES).contains(type);
	}

	private boolean isHtml() {
		return Arrays.asList(HTML_TYPES).contains(type);
	}

}
