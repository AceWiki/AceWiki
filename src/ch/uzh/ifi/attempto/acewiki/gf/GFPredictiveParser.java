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
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.common.base.Joiner;

import ch.uzh.ifi.attempto.base.ConcreteOption;
import ch.uzh.ifi.attempto.base.NextTokenOptions;
import ch.uzh.ifi.attempto.base.PredictiveParser;
import ch.uzh.ifi.attempto.base.SimpleConcreteOption;
import ch.uzh.ifi.attempto.base.SimpleNextTokenOptions;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;

/**
 * This is a predictive parser connecting to the JPGF implementation of GF.
 * 
 * @author Kaarel Kaljurand
 */
public class GFPredictiveParser implements PredictiveParser {

	private List<String> tokens = new ArrayList<String>();
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


	private void update() {
		// lazy parsing
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

	public int getTokenCount() {
		return tokens.size();
	}

	public NextTokenOptions getNextTokenOptions() {
		if (nextTokenOptions == null) {
			Set<ConcreteOption> options = new HashSet<ConcreteOption>();
			try {
				Set<String> completions = gfGrammar.complete(tokens, language);
				for (String s : completions) {
					Collection<String> cats = gfGrammar.getTokenToCats(language).get(s);
					if (cats.size() == 0) {
						options.add(new SimpleConcreteOption(s));
					}
					for (String c : cats) {
						options.add(new SimpleConcreteOption(s, c));
					}
				}
				nextTokenOptions = new SimpleNextTokenOptions(options);
			} catch (GfServiceException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return nextTokenOptions;
	}

	public boolean isPossibleNextToken(String token) {
		return getNextTokenOptions().containsToken(token);
	}


	/**
	 * <p>TODO: one should distinguish between "complete" and "parsable".
	 * E.g. an ACE text is never complete, because one can always add another
	 * sentence creating another ACE text. An incomplete ACE can be parsable
	 * though, e.g. a one-sentence ACE text.</p>
	 *
	 * @return <code>true</code> iff the current text is parsable
	 */
	public boolean isComplete() {
		try {
			return ! gfGrammar.parse(Joiner.on(" ").join(tokens), language).isEmpty();
		} catch (GfServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return false;
	}

	public int getReference() {
		return -1;
	}

}
