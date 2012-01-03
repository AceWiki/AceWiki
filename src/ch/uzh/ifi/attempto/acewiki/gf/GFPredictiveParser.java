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

package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.grammaticalframework.parser.ParseState;

import ch.uzh.ifi.attempto.base.ConcreteOption;
import ch.uzh.ifi.attempto.base.NextTokenOptions;
import ch.uzh.ifi.attempto.base.PredictiveParser;
import ch.uzh.ifi.attempto.base.SimpleConcreteOption;
import ch.uzh.ifi.attempto.base.SimpleNextTokenOptions;

/**
 * This is a predictive parser connecting to the JPGF implementation of GF.
 * 
 * @author Tobias Kuhn
 */
public class GFPredictiveParser implements PredictiveParser {
	
	private List<String> tokens = new ArrayList<String>();
	private ParseState parseState;
	private NextTokenOptions nextTokenOptions;
	private GFGrammar gfGrammar;
	private String language;
	
	/**
	 * Creates a new parser object for the given language.
	 * 
	 * @param gfGrammar The grammar object.
	 * @param language The language.
	 */
	public GFPredictiveParser(GFGrammar gfGrammar, String language) {
		this.gfGrammar = gfGrammar;
		this.language = language;
		update();
	}
	
	private ParseState getParseState() {
		if (parseState == null) {
			parseState = gfGrammar.parse(getTokensArray(), language);
		}
		return parseState;
	}
	
	private void update() {
		// lazy parsing
		parseState = null;
		nextTokenOptions = null;
	}
	
	public void addToken(String token) {
		tokens.add(token);
		update();
	}
	
	public void addTokens(List<String> tokens) {
		this.tokens.addAll(tokens);
		update();
	}
	
	public void removeToken() {
		tokens.remove(tokens.size()-1);
		update();
	}
	
	public void removeAllTokens() {
		tokens.clear();
		update();
	}
	
	public void setTokens(List<String> tokens) {
		this.tokens.clear();
		this.tokens.addAll(tokens);
		update();
	}
	
	public List<String> getTokens() {
		return tokens;
	}
	
	private String[] getTokensArray() {
		return tokens.toArray(new String[] {});
	}
	
	public int getTokenCount() {
		return tokens.size();
	}
	
	public NextTokenOptions getNextTokenOptions() {
		if (nextTokenOptions == null) {
			Set<ConcreteOption> options = new HashSet<ConcreteOption>();
			for (String s : getParseState().predict()) {
				options.add(new SimpleConcreteOption(s));
			}
			nextTokenOptions = new SimpleNextTokenOptions(options);
		}
		return nextTokenOptions;
	}
	
	public boolean isPossibleNextToken(String token) {
		return getNextTokenOptions().containsToken(token);
	}
	
	public boolean isComplete() {
		return getParseState().getTrees().length > 0;
	}
	
	public int getReference() {
		return -1;
	}

}
