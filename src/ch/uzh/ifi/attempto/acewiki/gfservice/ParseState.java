package ch.uzh.ifi.attempto.acewiki.gfservice;

import java.util.Set;

import com.google.common.collect.ImmutableSet;

public class ParseState {

	private final ImmutableSet<String> mTrees;

	ParseState(Iterable<String> trees) {
		mTrees = ImmutableSet.copyOf(trees);
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