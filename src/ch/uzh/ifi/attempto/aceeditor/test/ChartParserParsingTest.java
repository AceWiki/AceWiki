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

package ch.uzh.ifi.attempto.aceeditor.test;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import ch.uzh.ifi.attempto.chartparser.ChartParser;

/**
 * Parsing test of the ACE Editor grammar.
 * 
 * @author Tobias Kuhn
 */
public class ChartParserParsingTest {
	
	private static ChartParser chartparser = new ChartParser(new TestGrammar(), "test");
	private static BufferedReader in;
	private static int count = 0;
	
	/**
	 * Starts the test.
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		try {
	        in = new BufferedReader(new FileReader("src/ch/uzh/ifi/attempto/aceeditor/test/sentences.txt"));
	    } catch (IOException ex) {
	    	ex.printStackTrace();
	    }
		System.err.print("\n0");
		long timestart = System.currentTimeMillis();
		parseSentences();
		long timeend = System.currentTimeMillis();
		System.err.print("\n\nTime needed in seconds: " + (timeend-timestart)/1000.0 + "\n");
		try {
			in.close();
	    } catch (IOException ex) {
	    	ex.printStackTrace();
	    }
	}
	
	private static void parseSentences() {
		while (true) {
			String line;
			try {
				line = in.readLine();
			} catch (IOException ex) {
				ex.printStackTrace();
				break;
			}
			if (line == null) break;
			line = line.replaceFirst(" *$", "");
			chartparser.removeAllTokens();
			for (String t : line.split("  ")) {
				chartparser.addToken(t);
			}
			if (!chartparser.isComplete()) {
				System.err.println("ERROR: text could not be parsed: " + line);
			}
			count++;
			if (count % 1000 == 0) {
				System.err.print("\n" + count);
			} else if (count % 100 == 0) {
				System.err.print(".");
			}
		}
	}

}
