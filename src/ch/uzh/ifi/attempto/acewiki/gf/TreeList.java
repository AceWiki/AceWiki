package ch.uzh.ifi.attempto.acewiki.gf;

import java.util.List;

import com.google.common.collect.ImmutableList;

public class TreeList {

	private final ImmutableList<String> mTrees;

	public TreeList(Iterable<String> trees) {
		mTrees = ImmutableList.copyOf(trees);
	}

	public TreeList(String tree) {
		mTrees = ImmutableList.of(tree);
	}

	public List<String> getTrees() {
		return mTrees;
	}

	public int size() {
		return mTrees.size();
	}

	public String toString() {
		return mTrees.toString();
	}

}