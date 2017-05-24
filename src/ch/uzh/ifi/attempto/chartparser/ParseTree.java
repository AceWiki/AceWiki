// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class represents the parse tree of a successfully parsed text.
 * 
 * @author Tobias Kuhn
 */
public class ParseTree {
	
	private ParseTreeNode topNode;
	private String lamFunctor = "lam";
	private String appFunctor = "app";
	private String concatFunctor = "+";
	private String semLabel = "sem";
	
	/**
	 * Creates a new parse tree object.
	 * 
	 * @param topNode The top node.
	 */
	ParseTree(Edge edge) {
		this.topNode = new ParseTreeNode(edge.deepCopy(true));
	}
	
	private ParseTree(ParseTreeNode topNode) {
		this.topNode = topNode;
	}
	
	private ParseTree createParseTree(ParseTreeNode topNode) {
		ParseTree newParseTree = new ParseTree(topNode);
		newParseTree.lamFunctor = lamFunctor;
		newParseTree.appFunctor = appFunctor;
		newParseTree.concatFunctor = concatFunctor;
		newParseTree.semLabel = semLabel;
		return newParseTree;
	}
	
	/**
	 * Returns the start position of this tree. This can be relevant for subtrees.
	 * 
	 * @return The start position.
	 */
	public int getStartPos() {
		return topNode.getStartPos();
	}
	
	/**
	 * Returns the end position of this tree. This can be relevant for subtrees.
	 * 
	 * @return The end position.
	 */
	public int getEndPos() {
		return topNode.getEndPos();
	}
	
	/**
	 * Sets the name of the annotation item that contains the semantics information. The default is
	 * "sem".
	 * 
	 * @param semLabel The name of the annotation item containing the semantics.
	 */
	public void setSemanticsLabel(String semLabel) {
		this.semLabel = semLabel;
	}
	
	/**
	 * Sets the functor of the lambda function for the calculation of lambda semantics. The default
	 * is "lam".
	 * 
	 * @param lamFunctor The lambda functor.
	 */
	public void setLambdaFunctor(String lamFunctor) {
		this.lamFunctor = lamFunctor;
	}
	
	/**
	 * Sets the functor of the application function for the calculation of lambda semantics. The
	 * default is "app".
	 * 
	 * @param appFunctor The application functor.
	 */
	public void setApplicationFunctor(String appFunctor) {
		this.appFunctor = appFunctor;
	}

	/**
	 * Sets the functor of the concatenation function for the calculation of semantics. The default
	 * is "+". Terms using the concatenation functor are flattened. The functor can be set to null
	 * to avoid flattening.
	 * 
	 * @param concatFunctor The concatentation functor.
	 */
	public void setConcatFunctor(String concatFunctor) {
		this.concatFunctor = concatFunctor;
	}
	
	/**
	 * Returns the top node of the parse tree.
	 * 
	 * @return The top node of the parse tree.
	 */
	public ParseTreeNode getTopNode() {
		return topNode;
	}
	
	/**
	 * Returns the syntax tree. The leaves of the tree are objects of Category. All other nodes are
	 * arrays of Object containing the child nodes.
	 * 
	 * @return The syntax tree.
	 */
	public Object getSynTree() {
		return getSynTree(topNode);
	}
	
	private Object getSynTree(ParseTreeNode n) {
		Category h = n.getCategory();
		List<ParseTreeNode> c = n.getChildren();
		Object[] o = new Object[c.size()+1];
		o[0] = h;
		for (int i = 0 ; i < c.size() ; i++) {
			o[i+1] = getSynTree(c.get(i));
		}
		return o;
	}
	
	/**
	 * Returns a serialization of the syntax tree.
	 * 
	 * @return A serialization of the syntax tree.
	 */
	public String getSerializedSynTree() {
		return serializeStructure(getSynTree());
	}
	
	/**
	 * Returns an ASCII representation of the syntax tree.
	 * 
	 * @return An ASCII representation of the syntax tree.
	 */
	public String getAsciiSynTree() {
		return structureToAsciiTree(getSynTree(), 0);
	}

	/**
	 * Returns the semantics tree. The semantics are retrieved from the annotations of the grammar
	 * rules. The leaves of the tree are objects of String or StringRef. All other nodes are arrays
	 * of Object containing the child nodes.
	 * 
	 * @return The semantics tree.
	 */
	public Object getSemTree() {
		Object o = getSemTree(topNode);
		if (concatFunctor != null) {
			o = applyConcatenation(o);
		}
		return o;
	}
	
	private Object getSemTree(ParseTreeNode n) {
		Object structure =  n.getAnnotation().getItem(semLabel);
		if (structure == null) return null;
		for (ParseTreeNode c : n.getChildren()) {
			Object o = getSemTree(c);
			if (o != null) {
				structure = new Object[] {appFunctor, structure, o};
			}
		}
		return structure;
	}

	/**
	 * Returns a serialization of the semantics tree.
	 * 
	 * @return A serialization of the semantics tree.
	 */
	public String getSerializedSemTree() {
		return serializeStructure(getSemTree());
	}

	/**
	 * Returns an ASCII representation of the semantics tree.
	 * 
	 * @return An ASCII representation of the semantics tree.
	 */
	public String getAsciiSemTree() {
		return structureToAsciiTree(getSemTree(), 0);
	}

	/**
	 * Returns the semantics tree, interpreted as a lambda expression. The returned tree is beta-
	 * reduced. The semantics are retrieved from the annotations of the grammar rules. The leaves
	 * of the tree are objects of String or StringRef. All other nodes are arrays of Object
	 * containing the child nodes.
	 * 
	 * @return The beta-reduced semantics tree.
	 */
	public Object getLambdaSemTree() {
		Object o = getSemTree(topNode);
		Map<Integer, Object> replace = new HashMap<Integer, Object>();
		replace.put(-1, "");
		while (replace.containsKey(-1)) {
			replace.remove(-1);
			o = applyBetaReduction(o, replace);
		}
		if (concatFunctor != null) {
			o = applyConcatenation(o);
		}
		return o;
	}

	/**
	 * Returns a serialization of the semantics tree under lambda interpretation.
	 * (new optimized implementation)
	 * 
	 * @return A serialization of the lambda semantics tree.
	 */
	public String getSerializedLambdaSemTree() {
		return new SemanticTreeProcessor(this).reduce().getSemantics();
	}

	/**
	 * The old implementation of getSerializedLambdaSemTree, just in case problems
	 * pop up with the new one.
	 * 
	 * @return A serialization of the lambda semantics tree.
	 */
	@Deprecated
	public String getOldSerializedLambdaSemTree() {
		return serializeStructure(getLambdaSemTree());
	}
	
	/**
	 * Returns an ASCII representation of the semantics tree under lambda interpretation.
	 * 
	 * @return An ASCII representation of the lambda semantics tree.
	 */
	public String getAsciiLambdaSemTree() {
		return structureToAsciiTree(getLambdaSemTree(), 0);
	}
	
	/**
	 * Returns all subtrees that have the given category name as their top node. In the case of
	 * nested nodes with the given category, only the top-most subtree (containing all other
	 * potential subtrees) is returned.
	 * 
	 * @param categoryName The category name.
	 * @return A list of all matching subtrees.
	 */
	public List<ParseTree> getSubTrees(String categoryName) {
		List<ParseTreeNode> topNodeList = new ArrayList<ParseTreeNode>();
		topNodeList.add(topNode);
		List<ParseTree> subTrees = new ArrayList<ParseTree>();
		collectSubTrees(categoryName, topNodeList, subTrees);
		return subTrees;
	}
	
	private void collectSubTrees(String categoryName, List<ParseTreeNode> nodes,
			List<ParseTree> subTrees) {
		for (ParseTreeNode n : nodes) {
			if (n.getCategory().getName().equals(categoryName)) {
				subTrees.add(createParseTree(n));
			} else {
				collectSubTrees(categoryName, n.getChildren(), subTrees);
			}
		}
	}
	
	/**
	 * Returns the list of terminals of this tree.
	 * 
	 * @return The list of terminals.
	 */
	public List<Terminal> getTerminals() {
		return topNode.getTerminals();
	}
	
	private Object applyBetaReduction(Object obj, Map<Integer, Object> replace) {
		// TODO improve this (just one run, see Blackburn & Bos)
		if (obj == null) {
			return null;
		} else if (obj instanceof String) {
			return obj;
		} else if (obj instanceof StringRef) {
			StringRef sr = (StringRef) obj;
			if (replace.containsKey(sr.getID())) {
				replace.put(-1, "");
				return applyBetaReduction(replace.get(sr.getID()), replace);
			} else {
				return obj;
			}
		} else if (obj instanceof Object[]) {
			Object[] a = (Object[]) obj;
			if (a.length == 0) {
				return obj;
			}
			Object[] c = new Object[a.length];
			for (int i = 0 ; i < a.length ; i++) {
				c[i] = applyBetaReduction(a[i], replace);
			}
			if (a.length == 3 && appFunctor.equals(a[0]) && a[1] instanceof Object[]) {
				Object[] l = (Object[]) a[1];
				if (l.length == 3 && lamFunctor.equals(l[0]) && l[1] instanceof StringRef) {
					replace.put(((StringRef) l[1]).getID(), a[2]);
					replace.put(-1, "");
					return applyBetaReduction(l[2], replace);
				}
			}
			return c;
		}
		return obj;
	}
	
	private String serializeStructure(Object obj) {
		if (obj instanceof Object[]) {
			Object[] a = (Object[]) obj;
			if (a.length == 0) {
				return "*empty*";
			} else if (a.length == 1) {
				return elementToString(a[0]);
			} else {
				String s = elementToString(a[0]) + "(";
				for (int i = 1 ; i < a.length ; i++) {
					s += serializeStructure(a[i]) + ", ";
				}
				if (s.endsWith(", ")) s = s.substring(0, s.length()-2);
				return s + ")";
			}
		} else {
			return elementToString(obj);
		}
	}
	
	private String structureToAsciiTree(Object obj, int tab) {
		String t = "";
		for (int i=0 ; i < tab ; i++) t += "  ";
		if (obj instanceof Object[]) {
			Object[] a = (Object[]) obj;
			if (a.length == 0) {
				t += "*empty*\n";
			} else {
				t += elementToString(a[0]) + "\n";
				for (int i = 1 ; i < a.length ; i++) {
					t += structureToAsciiTree(a[i], tab+1);
				}
			}
		} else {
			return t + elementToString(obj) + "\n";
		}
		return t;
	}
	
	private String elementToString(Object obj) {
		if (obj == null) {
			return "*null*";
		} else if (obj instanceof String) {
			String s = (String) obj;
			if (s.matches("[a-zA-Z0-9_]+")) {
				return s;
			} else {
				return "'" + s + "'";
			}
		} else if (obj instanceof StringRef) {
			StringRef sr = (StringRef) obj;
			if (sr.getString() == null) {
				return "?" + sr.getID();
			} else {
				return elementToString(sr.getString());
			}
		} else if (obj instanceof Terminal) {
			return obj.toString();
		} else if (obj instanceof Preterminal) {
			return "$" + ((Preterminal) obj).getName();
		} else if (obj instanceof Category) {
			return ((Category) obj).getName();
		}
		return "*invalid*";
	}
	
	private Object applyConcatenation(Object obj) {
		if (isConcatFunction(obj)) {
			Object[] a = (Object[]) obj;
			List<Object> cl = new ArrayList<Object>();
			for (int i = 1 ; i < a.length ; i++) {
				Object ci = applyConcatenation(a[i]);
				if (isConcatFunction(ci)) {
					Object[] ai = (Object[]) ci;
					for (int j = 1 ; j < ai.length ; j++) {
						addFlat(cl, ai[j]);
					}
				} else if (!"".equals(ci)) {
					addFlat(cl, ci);
				}
			}
			if (cl.size() == 0) {
				return "";
			} else if (cl.size() == 1) {
				return cl.get(0);
			} else {
				cl.add(0, concatFunctor);
				return cl.toArray();
			}
		} else if (obj instanceof Object[]) {
			Object[] a = (Object[]) obj;
			Object[] c = new Object[a.length];
			for (int i = 0 ; i < a.length ; i++) {
				c[i] = applyConcatenation(a[i]);
			}
			return c;
		} else {
			return obj;
		}
	}
	
	private boolean isConcatFunction(Object obj) {
		if (!(obj instanceof Object[])) return false;
		Object[] a = (Object[]) obj;
		if (a.length == 0) return false;
		if (!(a[0] instanceof String)) return false;
		if (!a[0].toString().equals(concatFunctor)) return false;
		return true;
	}
	
	private void addFlat(List<Object> list, Object obj) {
		if (obj instanceof StringRef && ((StringRef) obj).getString() != null) {
			obj = ((StringRef) obj).getString();
		}
		if (list.isEmpty() || !(obj instanceof String)) {
			list.add(obj);
		} else {
			if (list.get(list.size()-1) instanceof String) {
				String s = (String) list.remove(list.size()-1);
				list.add(s + obj);
			} else {
				list.add(obj);
			}
		}
	}

}
