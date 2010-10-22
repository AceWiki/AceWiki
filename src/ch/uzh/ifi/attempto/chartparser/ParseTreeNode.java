// This file is part of AceWiki.
// Copyright 2008-2010, Tobias Kuhn.
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
 * grammar rule.
 * 
 * @author Tobias Kuhn
 */
public class ParseTreeNode {
	
	private Category category;
	private Annotation annotation;
	private List<ParseTreeNode> children = new ArrayList<ParseTreeNode>();
	
	/**
	 * Creates a new parse tree node out of the given edge.
	 * 
	 * @param edge The edge.
	 */
	ParseTreeNode(Edge edge) {
		this.category = edge.getHead();
		this.annotation = edge.getAnnotation();
		for (int i = 0 ; i < edge.getBody().length ; i++) {
			Edge e = edge.getLinks().get(i);
			if (e != null) {
				try {
					e.getHead().unify(edge.getBody()[i]);
					children.add(new ParseTreeNode(e));
				} catch (UnificationFailedException ex) {
					throw new RuntimeException("Unexpected unification error", ex);
				}
			} else {
				children.add(new ParseTreeNode(edge.getBody()[i]));
			}
		}
	}
	
	/**
	 * Creates a new parse tree node (without children) out of the given category.
	 * 
	 * @param category The category.
	 */
	ParseTreeNode(Category category) {
		this.category = category;
		this.annotation = new Annotation();
	}
	
	/**
	 * Adds a child to this node.
	 * 
	 * @param child The child node to be added.
	 */
	void addChild(ParseTreeNode child) {
		children.add(child);
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

}
