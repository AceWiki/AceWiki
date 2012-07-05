package ch.uzh.ifi.attempto.base;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.google.common.collect.ImmutableSet;

public class TextContainerSet implements Iterable<TextContainer> {

	private final Set<TextContainer> mTextContainerSet;

	public TextContainerSet(TextContainer... textContainers) {
		mTextContainerSet = ImmutableSet.copyOf(textContainers);
	}

	public TextContainerSet(Collection<TextContainer> textContainers) {
		mTextContainerSet = ImmutableSet.copyOf(textContainers);
	}

	// TODO: this is temporary
	// @deprecated
	public List<TextElement> getTextElements() {
		return mTextContainerSet.iterator().next().getTextElements();
	}

	@Override
	public Iterator<TextContainer> iterator() {
		return mTextContainerSet.iterator();
	}

}