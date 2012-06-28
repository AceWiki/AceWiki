package ch.uzh.ifi.attempto.acewiki.gfservice;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class ParseState {

	private final Set<String> mTrees;

	ParseState(String[] trees) {
		this(new HashSet<String>(Arrays.asList(trees)));
	}

	// TODO: how to convert an Iterable into a Set in a nice way
	ParseState(Iterable<String> trees) {
		mTrees = new HashSet<String>();
		for (String t : trees) {
			mTrees.add(t);
		}
	}

	ParseState(Set<String> trees) {
		mTrees = trees;
	}

	public Set<String> getTrees() {
		return mTrees;
	}

	public int size() {
		return mTrees.size();
	}

	public String toString() {
		return mTrees.toString();
	}

}