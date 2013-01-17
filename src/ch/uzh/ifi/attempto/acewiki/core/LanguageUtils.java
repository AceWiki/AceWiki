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

package ch.uzh.ifi.attempto.acewiki.core;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * This utility class contains static methods for general tasks on the language level.
 * 
 * @author Tobias Kuhn
 */
public class LanguageUtils {
	
	private static OntologyElementsComparator comparator = new OntologyElementsComparator();
	
	// no instances allowed:
	private LanguageUtils() {}

	/**
	 * Returns the pretty-printed form of the given text. Underscores are transformed into blanks.
	 * 
	 * @param text The input text.
	 * @return The pretty-printed text;
	 */
	public static String getPrettyPrinted(String text) {
		if (text == null) return null;
		return text.replace("_", " ");
	}
	
	/**
	 * Returns a heading of the form "headword1 (headword2, ..., headwordn)" for the given ontology
	 * element.
	 * 
	 * @param oe The ontology element.
	 * @return The heading.
	 */
	public static String getHeading(OntologyElement oe) {
		String[] h = oe.getHeadwords();
		String heading = getPrettyPrinted(h[0]);
		if (h.length > 1) {
			String aliases = "";
			for (int i=1 ; i<h.length ; i++) {
				aliases += ", " + getPrettyPrinted(h[i]);
			}
			if (aliases.length() > 0) {
				aliases = aliases.substring(2);
			}
			heading += " (" + aliases + ")";
		}
		return heading;
	}
	
	/**
	 * Sorts the list of ontology elements according to their main headword.
	 * 
	 * @param elements The list to be sorted.
	 */
	public static void sortOntologyElements(List<? extends OntologyElement> elements) {
		Collections.sort(elements, comparator);
	}
	
	
	private static class OntologyElementsComparator implements Comparator<OntologyElement> {

		public int compare(OntologyElement o1, OntologyElement o2) {
			if (o1 instanceof GeneralTopic && !(o2 instanceof GeneralTopic)) {
				return -1;
			} else if (!(o1 instanceof GeneralTopic) && o2 instanceof GeneralTopic) {
				return 1;
			} else {
				return o1.getHeadwords()[0].compareToIgnoreCase(o2.getHeadwords()[0]);
			}
		}
		
	}

}
