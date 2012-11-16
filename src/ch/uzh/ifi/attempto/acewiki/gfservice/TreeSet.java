package ch.uzh.ifi.attempto.acewiki.gfservice;

import java.util.Set;

import com.google.common.collect.ImmutableSet;

public class TreeSet {

	private final ImmutableSet<String> mTrees;

	public TreeSet(Iterable<String> trees) {
		mTrees = ImmutableSet.copyOf(trees);
	}

	public TreeSet(String tree) {
		mTrees = ImmutableSet.of(tree);
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