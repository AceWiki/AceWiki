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

import static ch.uzh.ifi.attempto.ape.OutputType.PARAPHRASE1;
import static ch.uzh.ifi.attempto.ape.OutputType.SYNTAXPP;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.ontology.Sentence;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.ape.SyntaxBoxes;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import echopointng.DirectHtml;

/**
 * This class represents a page that shows the details of an ACE sentence.
 * 
 * @author Tobias Kuhn
 */
public class SentencePage extends WikiPage implements ActionListener {

	private static final long serialVersionUID = -1550505465878272821L;

	private Sentence sentence;

	/**
	 * Creates a new sentence page.
	 * 
	 * @param wiki The wiki instance.
	 * @param sentence The sentence to be shown in the page.
	 */
	public SentencePage(Wiki wiki, Sentence sentence) {
		super(wiki);
		this.sentence = sentence;
		
		addSelectedTab("Sentence");
		addTab("Logic", this);
		
		add(new Title(sentence.getPrettyText(), false));
		addHorizontalLine();
		add(new VSpace(15));
		
		addHeadline("Paraphrase");
		
		Column paraphraseColumn = new Column();
		paraphraseColumn.setInsets(new Insets(10, 5, 5, 15));
		paraphraseColumn.add(new Label(sentence.getParserResult().get(PARAPHRASE1)));
		add(paraphraseColumn);
		
		addHeadline("Syntax Boxes");
		
		Column boxesColumn = new Column();
		boxesColumn.setInsets(new Insets(10, 5, 5, 15));
		boxesColumn.add(new DirectHtml(SyntaxBoxes.getBoxesHtml(sentence.getParserResult())));
		add(boxesColumn);
		
		addHeadline("Syntax Tree");
		
		Column syntaxColumn = new Column();
		syntaxColumn.setInsets(new Insets(10, 0, 5, 15));
		syntaxColumn.add(new DirectHtml(
				"<pre>" + sentence.getParserResult().get(SYNTAXPP) + "</pre>"
			));
		add(syntaxColumn);
	}

	public void actionPerformed(ActionEvent e) {
		if ("Logic".equals(e.getActionCommand())) {
			getWiki().showPage(new LogicPage(getWiki(), sentence));
		}
	}

	public boolean equals(Object obj) {
		if (obj instanceof SentencePage) {
			return sentence == ((SentencePage) obj).sentence;
		}
		return false;
	}
	
	public String toString() {
		return sentence.getText();
	}

}
