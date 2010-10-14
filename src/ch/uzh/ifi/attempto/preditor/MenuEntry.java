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

package ch.uzh.ifi.attempto.preditor;

import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import ch.uzh.ifi.attempto.chartparser.ConcreteOption;
import ch.uzh.ifi.attempto.chartparser.Preterminal;
import ch.uzh.ifi.attempto.echocomp.Style;

/**
 * This class represents a menu item that contains a text element. The text element is added to
 * the partial sentence when the user clicks on a menu entry in the predictive editor.
 * 
 * @author Tobias Kuhn
 */
public class MenuEntry extends MenuItem {

	private static final long serialVersionUID = -4231372412315340523L;
	
	private TextElement textElement;
	private boolean strong = true;
	private boolean highlighted = false;
	
	/**
	 * Creates a new menu entry for the given word.
	 * 
	 * @param word The word.
	 * @param category The category of the word.
	 * @param menuGroup The menu group to which this entry should be assigned.
	 */
	public MenuEntry(String word, Preterminal category, String menuGroup) {
		this(new TextElement(word, category), menuGroup);
	}
	
	/**
	 * Creates a new menu entry for the given word.
	 * 
	 * @param word The word.
	 * @param categoryName The name of the category of the word.
	 * @param menuGroup The menu group to which this entry should be assigned.
	 */
	public MenuEntry(String word, String categoryName, String menuGroup) {
		this(new TextElement(word, categoryName), menuGroup);
	}
	
	/**
	 * Creates a new menu entry for the given word.
	 * 
	 * @param word The word.
	 * @param menuGroup The menu group to which this entry should be assigned.
	 */
	public MenuEntry(String word, String menuGroup) {
		this(new TextElement(word), menuGroup);
	}
	
	/**
	 * Creates a new menu entry on the basis of the given text element.
	 * 
	 * @param textElement The text element.
	 * @param menuGroup The menu group to which this entry should be assigned.
	 */
	public MenuEntry(TextElement textElement, String menuGroup) {
		super(menuGroup);
        this.textElement = textElement;
		setText(textElement.getText());
		setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(12)));
    }
	
	/**
	 * Creates a new menu entry on the basis of the given concrete option.
	 * 
	 * @param cOption The concrete option.
	 * @param menuGroup The menu group to which this entry should be assigned.
	 */
	public MenuEntry(ConcreteOption cOption, String menuGroup) {
		this(cOption.getWord(), cOption.getCategory(), menuGroup);
	}
    
	/**
	 * Returns the text element.
	 * 
	 * @return The text element.
	 */
    public TextElement getTextElement() {
    	return textElement;
    }
    
    /**
     * This method determines whether the menu entry is highlighted or not. Hightlighted menu
     * entries are displayed in bold font and are shown in front of non-highlighted menu entries
     * in sorted lists.
     * 
     * @param highlighted true if this entry should be highlighted.
     */
    public void setHighlighted(boolean highlighted) {
    	this.highlighted = highlighted;
		if (highlighted) {
			setFont(new Font(Style.fontTypeface, Font.BOLD, new Extent(12)));
		} else {
			setFont(new Font(Style.fontTypeface, Font.PLAIN, new Extent(12)));
		}
    }

	public int compareTo(MenuItem m) {
		if (m instanceof MenuEntry) {
			MenuEntry other = (MenuEntry) m;
			if (this.strong && !other.strong) {
				return -1;
			} else if (!this.strong && other.strong) {
				return 1;
			} else {
				if (this.highlighted && !other.highlighted) {
					return -1;
				} else if (!this.highlighted && other.highlighted) {
					return 1;
				} else {
					return super.compareTo(m);
				}
			}
		} else {
			return super.compareTo(m);
		}
	}
	
	protected String[] getContent() {
		return new String[] {"entry", getText(), getMenuGroup()};
	}
    
}
