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

package ch.uzh.ifi.attempto.chartparser;

import java.util.Collection;

import ch.uzh.ifi.attempto.base.AbstractOption;

/**
 * This interface represents a dynamic lexicon that can be used by the chart parser.
 * 
 * @author Tobias Kuhn
 */
public interface DynamicLexicon {
	
	/**
	 * This method should return the lexical rules for the given abstract option. Lexical rules
	 * that do not comply with this abstract option are filtered out afterwards. This method is
	 * used for the prediction of possible next tokens. The returned collection does not need to be
	 * complete. For open word classes (e.g. string and numbers) just a subset of all possible
	 * lexicon entries can be returned.
	 * 
	 * @param option The abstract option.
	 * @return The lexical rules for the given abstract option.
	 */
	public Collection<LexicalRule> getLexRules(AbstractOption option);
	
	/**
	 * This method should return the lexical rules with the given word (terminal category). Lexical
	 * rules with a different word are filtered out afterwards.
	 * 
	 * @param word The word.
	 * @return The lexical rules with the given word as their terminal category.
	 */
	public Collection<LexicalRule> getLexRules(String word);

}
