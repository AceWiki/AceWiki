package ch.uzh.ifi.attempto.acewiki.aceowl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ch.uzh.ifi.attempto.acewiki.core.MenuEngine;

public class ACEOWLMenuEngine implements MenuEngine {
	
	private static List<String> menuGroups;
	private static List<String> extensibleCategories;

	static {
		menuGroups = new ArrayList<String>();
		menuGroups.add("function word");
		menuGroups.add("proper name");
		menuGroups.add("noun");
		menuGroups.add("plural noun");
		menuGroups.add("of-construct");
		menuGroups.add("transitive adjective");
		menuGroups.add("verb");
		menuGroups.add("passive verb");
		menuGroups.add("new variable");
		menuGroups.add("reference");
		
		extensibleCategories = new ArrayList<String>();
		extensibleCategories.add("propername");
		extensibleCategories.add("noun");
		extensibleCategories.add("nounpl");
		extensibleCategories.add("nounof");
		extensibleCategories.add("verbsg");
		extensibleCategories.add("verbinf");
		extensibleCategories.add("pverb");
		extensibleCategories.add("tradj");
	}

	public int getColorShift(String menuGroup) {
		if (menuGroup.equals("proper name")) return 60;
		if (menuGroup.equals("noun")) return 100;
		if (menuGroup.equals("plural noun")) return 120;
		if (menuGroup.equals("of-construct")) return 140;
		if (menuGroup.equals("transitive adjective")) return 180;
		if (menuGroup.equals("verb")) return 210;
		if (menuGroup.equals("passive verb")) return 210;
		if (menuGroup.equals("new variable")) return 320;
		if (menuGroup.equals("reference")) return 320;
		return 0;
	}

	public List<String> getMenuGroups() {
		return menuGroups;
	}

	public String getMenuGroup(String c) {
		if (c == null) return "function word";
		if (c.equals("propername")) return "proper name";
		if (c.equals("noun")) return "noun";
		if (c.equals("defnoun")) return "reference";
		if (c.equals("nounpl")) return "plural noun";
		if (c.equals("nounof")) return "of-construct";
		if (c.equals("verbsg")) return "verb";
		if (c.equals("verbinf")) return "verb";
		if (c.equals("pverb")) return "passive verb";
		if (c.equals("tradj")) return "transitive adjective";
		if (c.equals("variable")) return "new variable";
		if (c.equals("reference")) return "reference";
		return "function word";
	}

	public List<String> getExtensibleCategories() {
		return extensibleCategories;
	}

	public String getWordType(String c) {
		if (c.equals("propername")) return "propername";
		if (c.equals("noun")) return "noun";
		if (c.equals("nounpl")) return "noun";
		if (c.equals("nounof")) return "nounof";
		if (c.equals("verbsg")) return "trverb";
		if (c.equals("verbinf")) return "trverb";
		if (c.equals("pverb")) return "trverb";
		if (c.equals("tradj")) return "tradj";
		return null;
	}

	public int getWordNumber(String c) {
		if (c.equals("nounpl")) return 1;
		if (c.equals("verbinf")) return 1;
		if (c.equals("pverb")) return 2;
		return 0;
	}

}
