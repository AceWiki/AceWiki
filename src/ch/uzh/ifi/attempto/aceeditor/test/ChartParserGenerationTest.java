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

package ch.uzh.ifi.attempto.aceeditor.test;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import ch.uzh.ifi.attempto.base.ConcreteOption;
import ch.uzh.ifi.attempto.chartparser.ChartParser;

/**
 * Language generation test of the ACE Editor grammar.
 * 
 * @author Tobias Kuhn
 */
public class ChartParserGenerationTest {
	
	private static ChartParser chartparser = new ChartParser(new TestGrammar(), "test");
	private static BufferedWriter out;
	private static int count = 0;
	
	/**
	 * Starts the test.
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		try {
	        out = new BufferedWriter(new FileWriter("src/ch/uzh/ifi/attempto/aceeditor/test/sentences_cp.txt"));
	    } catch (IOException ex) {
	    	ex.printStackTrace();
	    }
		System.err.print("\n0");
		long timestart = System.currentTimeMillis();
		completeSentence();
		long timeend = System.currentTimeMillis();
		System.err.print("\n\nTime needed in seconds: " + (timeend-timestart)/1000.0 + "\n");
		try {
			out.close();
	    } catch (IOException ex) {
	    	ex.printStackTrace();
	    }
	}
	
	private static void completeSentence() {
		if (chartparser.getTokens().size() == 7) {
			if (chartparser.isComplete()) printTokens();
			return;
		}
		
		List<String> processed = new ArrayList<String>();
		for (ConcreteOption o : chartparser.getConcreteOptions()) {
			String n = o.getWord();
			if (processed.contains(n)) continue;
			processed.add(n);
			chartparser.addToken(o.getWord());
			completeSentence();
			chartparser.removeToken();
		}
	}
	
	private static void printTokens() {
		try {
			String s = "";
			for (String t : chartparser.getTokens()) {
				s += "  " + t;
			}
			out.write(s.substring(2) + "\n");
			
			count++;
			if (count % 1000 == 0) {
				System.err.print("\n" + count);
			} else if (count % 100 == 0) {
				System.err.print(".");
			}
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}

}
