package ch.uzh.ifi.attempto.acewiki.gfservice;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class ParseState {

	private final Set<String> mTrees;
	private final String mTree;

	ParseState(String[] trees) {
		this(new HashSet<String>(Arrays.asList(trees)));
	}

	// TODO: how to convert an Iterable into a Set in a nice way
	ParseState(Iterable<String> trees) {
		mTrees = new HashSet<String>();
		for (String t : trees) {
			mTrees.add(t);
		}
		if (mTrees.isEmpty()) {
			mTree = "";
		} else {
			mTree = mTrees.iterator().next();
		}
	}

	ParseState(Set<String> trees) {
		mTrees = trees;
		if (trees.isEmpty()) {
			mTree = "";
		} else {
			mTree = trees.iterator().next();
		}
	}

	public Set<String> getTrees() {
		return mTrees;
	}

	public int size() {
		return mTrees.size();
	}

	public String getTree() {
		return mTree;
	}

	public String toString() {
		return mTrees.toString();
	}

}