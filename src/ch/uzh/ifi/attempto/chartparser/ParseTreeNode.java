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

package ch.uzh.ifi.attempto.chartparser;

import java.util.ArrayList;
import java.util.List;

/**
 * This class represents a node of the parse tree. Each node has a category and an annotation
 * object that are carried over from the edge in the chart parser and originate from the respective
 * grammar rule. Additionally, each parse node has start and end positions denoting the covered
 * part of the input text.
 * 
 * @author Tobias Kuhn
 */
public class ParseTreeNode {
	
	private final Category category;
	private final int startPos;
	private final int endPos;
	private final Annotation annotation;
	private final List<ParseTreeNode> children = new ArrayList<ParseTreeNode>();
	
	/**
	 * Creates a new parse tree node out of the given edge.
	 * 
	 * @param edge The edge.
	 */
	ParseTreeNode(Edge edge) {
		this.category = edge.getHead();
		this.startPos = edge.getStartPos();
		this.endPos = edge.getEndPos();
		this.annotation = edge.getAnnotation();
		int pos = edge.getStartPos();
		for (int i = 0 ; i < edge.getBody().length ; i++) {
			Edge e = edge.getLinks().get(i);
			if (e != null) {
				try {
					e.getHead().unify(edge.getBody()[i]);
					children.add(new ParseTreeNode(e));
				} catch (UnificationFailedException ex) {
					throw new RuntimeException("Unexpected unification error", ex);
				}
				pos = e.getEndPos();
			} else {
				children.add(new ParseTreeNode(edge.getBody()[i], pos));
			}
		}
	}
	
	/**
	 * Creates a new parse tree node (without children) out of the given category.
	 * 
	 * @param category The category.
	 */
	private ParseTreeNode(Category category, int pos) {
		this.category = category;
		this.startPos = pos;
		this.endPos = pos;
		this.annotation = new Annotation();
	}
	
	/**
	 * Returns the category of this node.
	 * 
	 * @return The category.
	 */
	public Category getCategory() {
		return category;
	}
	
	/**
	 * Returns the start position. 0 is the position before the first token, 1 the position after
	 * the first token, 2 the position after the second token, and so on.
	 * 
	 * @return The start position.
	 */
	public int getStartPos() {
		return startPos;
	}
	
	/**
	 * Returns the end position. 0 is the position before the first token, 1 the position after
	 * the first token, 2 the position after the second token, and so on.
	 * 
	 * @return The end position.
	 */
	public int getEndPos() {
		return endPos;
	}
	
	/**
	 * Returns the annotation object of this node.
	 * 
	 * @return The annotation object.
	 */
	public Annotation getAnnotation() {
		return annotation;
	}
	
	/**
	 * Returns the annotation item for the given annotation item name.
	 * 
	 * @param name The name of the annotation item.
	 * @return The value of the annotation item.
	 */
	public Object getAnnotationItem(String name) {
		return annotation.getItem(name);
	}
	
	/**
	 * Returns the children of this node.
	 * 
	 * @return The children.
	 */
	public List<ParseTreeNode> getChildren() {
		return children;
	}
	
	/**
	 * Returns the child at the given position.
	 * 
	 * @param i The position.
	 * @return The child.
	 */
	public ParseTreeNode getChild(int i) {
		return children.get(i);
	}
	
	/**
	 * Returns the list of terminals that are descendants of this node.
	 * 
	 * @return The list of terminals.
	 */
	public List<Terminal> getTerminals() {
		List<Terminal> terminals = new ArrayList<Terminal>();
		collectTerminals(terminals);
		return terminals;
	}
	
	private void collectTerminals(List<Terminal> terminals) {
		if (category instanceof Terminal) {
			terminals.add((Terminal) category);
		} else {
			for (ParseTreeNode n : getChildren()) {
				n.collectTerminals(terminals);
			}
		}
	}

}
