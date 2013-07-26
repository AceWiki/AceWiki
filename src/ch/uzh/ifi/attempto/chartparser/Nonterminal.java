// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

package ch.uzh.ifi.attempto.chartparser;

/**
 * This class stands for a nonterminal category. This class also includes special categories
 * like references, scope openers, and position operators.
 * 
 * @author Tobias Kuhn
 */
public class Nonterminal extends Category {
	
	/**
	 * Creates a new nonterminal category.
	 */
	protected Nonterminal() {
	}

	/**
	 * Creates a new nonterminal category.
	 * 
	 * @param name The name of the category.
	 */
	public Nonterminal(String name) {
		this.name = name;
		featureMap = new FeatureMap();
	}
	
	/**
	 * Sets the feature map of this category.
	 * 
	 * @param featureMap The new feature map.
	 */
	public void setFeatureMap(FeatureMap featureMap) {
		this.featureMap = featureMap;
	}
	
	protected String getType() {
		return "nonterm";
	}
	
	public String toString() {
		return name + featureMap.toString();
	}

}
