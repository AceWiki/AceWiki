// This file is part of AceWiki.
// Copyright 2008-2011, Tobias Kuhn.
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

/**
 * This class represents an edge of a chart to be used by a chart parser. Edges are derived from grammar
 * rules. An edge contains the head and body categories of the rule it is derived from. In addition, an
 * edge has a start position and an end position that define the range of the text that is covered.
 * Furthermore, every edge has a progress value that records how many of the body categories are parsed.
 * Edges that have unparsed categories are called "active" whereas edges for which all body categories are
 * parsed are called "passive". In order to capture accessibility constraints for anaphoric
 * references, edges are either scope-closing or not, like rules. Additionally, edges contain two lists that
 * store the feature maps of the accessible antecedents. The first list stores the antecedents from outside the
 * range of the edge (the external antecedent list) and the second list stores the antecedents from inside the
 * range of the edge (the internal antecedent list).
 * 
 * @see GrammarRule
 * @author Tobias Kuhn
 */
class Edge {
	
	private int startPos;
	private int endPos;
	private Category head;
	private Category[] externalAnteList;
	private Category[] internalAnteList;
	
	private boolean scopeclosing;
	
	private Category[] body;
	private int progress = 0;
	private String identifier;
	
	private Annotation annotation;
	
	private List<Edge> links;
	
	private Edge() {
	}
	
	/**
	 * Creates a new edge on the basis of a grammar rule.
	 * 
	 * @param pos Start and end position.
	 * @param rule The grammar rule that is the basis for the creation of the edge.
	 */
	Edge(int pos, GrammarRule rule) {
		this.startPos = pos;
		this.endPos = pos;
		this.head = rule.getHead();
		this.body = rule.getBody();
		this.externalAnteList = new Category[0];
		this.internalAnteList = new Category[0];
		this.scopeclosing = rule.isScopeClosing();
		this.annotation = rule.getAnnotation();
		this.links = new ArrayList<Edge>();
	}
	
	/**
	 * Creates a new edge on the basis of a grammar rule.
	 * 
	 * @param pos Start and end position.
	 * @param rule The grammar rule that is the basis for the creation of the edge.
	 * @param externalAnteList The list of external antecedents.
	 */
	Edge(int pos, GrammarRule rule, Category[] externalAnteList) {
		this.startPos = pos;
		this.endPos = pos;
		this.head = rule.getHead();
		this.body = rule.getBody();
		if (externalAnteList == null || rule.hasEmptyBody()) {
			this.externalAnteList = new Category[0];
		} else {
			this.externalAnteList = externalAnteList;
		}
		this.internalAnteList = new Category[0];
		this.scopeclosing = rule.isScopeClosing();
		this.annotation = rule.getAnnotation();
		this.links = new ArrayList<Edge>();
	}
	
	/**
	 * Creates a new edge on the basis of a lexical rule.
	 * 
	 * @param pos Start and end position.
	 * @param lexRule The lexical rule that is the basis for the creation of the edge.
	 */
	Edge(int pos, LexicalRule lexRule) {
		this.startPos = pos;
		this.endPos = pos;
		this.head = lexRule.getCategory();
		this.body = new Category[] {lexRule.getWord()};
		this.externalAnteList = new Category[0];
		this.internalAnteList = new Category[0];
		this.annotation = lexRule.getAnnotation();
		this.scopeclosing = false;
		this.links = new ArrayList<Edge>();
	}
	
	/**
	 * Creates a new edge on the basis of a head category. The end position is the start position
	 * plus one.
	 * 
	 * @param startPos Start position.
	 * @param head The head category.
	 */
	Edge(int startPos, Terminal head) {
		this.startPos = startPos;
		this.endPos = startPos + 1;
		this.head = head;
		this.body = new Category[0];
		this.externalAnteList = new Category[0];
		this.internalAnteList = new Category[0];
		this.annotation = new Annotation();
		this.scopeclosing = false;
		this.links = new ArrayList<Edge>();
	}
	
	/**
	 * Returns the start position of this edge.
	 * 
	 * @return The start position.
	 */
	public int getStartPos() {
		return startPos;
	}
	
	/**
	 * Returns the end position of this edge.
	 * 
	 * @return The end position.
	 */
	public int getEndPos() {
		return endPos;
	}
	
	/**
	 * Returns the head category of this edge.
	 * 
	 * @return The head category.
	 */
	public Category getHead() {
		return head;
	}
	
	/**
	 * Returns the body categories of this edge.
	 * 
	 * @return The body categories.
	 */
	public Category[] getBody() {
		return body;
	}
	
	/**
	 * Returns the progress value of this edge.
	 * 
	 * @return The progress value.
	 */
	public int getProgress() {
		return progress;
	}
	
	/**
	 * Returns the list of external antecedents.
	 * 
	 * @return The external antecedents.
	 */
	public Category[] getExternalAnteList() {
		return externalAnteList;
	}
	
	/**
	 * Returns the list of internal antecedents.
	 * 
	 * @return The internal antecedents.
	 */
	public Category[] getInternalAnteList() {
		return internalAnteList;
	}
	
	/**
	 * Returns a list that is a concatenation of the lists of external and internal antecedents.
	 * 
	 * @return A combined antecedents list.
	 */
	public Category[] getCombinedAnteList() {
		Category[] l = new Category[externalAnteList.length + internalAnteList.length];
		for (int i = 0 ; i < externalAnteList.length ; i++) {
			l[i] = externalAnteList[i];
		}
		for (int i = 0 ; i < internalAnteList.length ; i++) {
			l[externalAnteList.length + i] = internalAnteList[i];
		}
		return l;
	}
	
	/**
	 * Returns the annotation object for this edge.
	 * 
	 * @return The annotation object.
	 */
	public Annotation getAnnotation() {
		return annotation;
	}
	
	/**
	 * Returns true if this edge is active, i.e. has unparsed body categories.
	 * 
	 * @return true if this edge is active.
	 */
	public boolean isActive() {
		return body.length > progress;
	}
	
	/**
	 * Returns true if this edge is scope-closing, i.e. is derived from an scope-closing rule.
	 * 
	 * @return true if this edge is scope-closing.
	 */
	public boolean isScopeClosing() {
		return scopeclosing;
	}
	
	/**
	 * Returns true if this edge carries antecedent information. Edge with an empty body and edges
	 * that originate from lexical rules do not carry antecedent information. All other edges do.
	 * 
	 * @return true if this edge carries antecedent information.
	 */
	public boolean carriesAntecedentInformation() {
		return body.length == 0 || (head instanceof Preterminal);
	}
	
	/**
	 * Returns the first active category, i.e. the first body category that has not yet been
	 * parsed. null is returned for inactive edges.
	 * 
	 * @return The first active category.
	 */
	public Category getNextActive() {
		if (!isActive()) return null;
		return body[progress];
	}
	
	/**
	 * Returns the linked edges, i.e. the edges on the basis of which this edge has been completed.
	 * It is assumed that the grammar is unambiguous. For ambiguous grammars, the links might be
	 * incomplete or incorrect.
	 * 
	 * @return The edges that are children of this edge.
	 */
	public List<Edge> getLinks() {
		return links;
	}
	
	/**
	 * Moves the parsing progress forward to the given position. The given edge is stored as a link.
	 * No checking is done within this method whether this operation is actually possible according
	 * to the rules of chart parsing.
	 * 
	 * @param pos The end position to move forward to.
	 * @param edge The edge that is used for moving forward.
	 */
	void step(int pos, Edge edge) {
		if (!isActive()) {
			throw new RuntimeException("Passive edge");
		}
		progress++;
		endPos = pos;
		boolean restr = false;
		for (Category c : edge.getInternalAnteList()) {
			if (!edge.isScopeClosing()) {
				addAntecedents(c);
			} else if (c.getName().equals("//")) {
				restr = true;
			} else if (!restr || c.getName().equals(">>")) {
				addAntecedents(c);
			}
		}
		links.add(edge);
	}
	
	/**
	 * Moves the parsing progress one step forward while keeping the same end position.
	 * No checking is done within this method whether this operation is actually possible according
	 * to the rules of chart parsing.
	 */
	void step() {
		if (!isActive()) {
			throw new RuntimeException("Passive edge");
		}
		progress++;
		links.add(null);
	}
	
	/**
	 * Adds the feature maps to the list of internal antecedents.
	 * 
	 * @param newAnteList The feature maps of internal antecedents.
	 */
	void addAntecedents(Category... newAnteList) {
		Category[] oldInternalList = internalAnteList;
		int l1 = oldInternalList.length;
		int l2 = newAnteList.length;
		internalAnteList = new Category[l1 + l2];
		for (int i = 0 ; i < l1 ; i++) {
			internalAnteList[i] = oldInternalList[i];
		}
		for (int i = 0 ; i < l2 ; i++) {
			internalAnteList[l1 + i] = newAnteList[i];
		}
	}
	
	/**
	 * Unifies this edge with the given edge. If the unification fails, a
	 * UnificationFailedException is thrown. In this case, the two edges remain partly unified,
	 * i.e. no backtracking is done. Thus, this operation should be perfomed only if it is certain that
	 * the unification succeeds, or if the operation is performed on copies of objects that are not
	 * used anymore afterwards.
	 * 
	 * @param edge The edge to be unified with this edge.
	 * @throws UnificationFailedException If unification fails.
	 */
	void unify(Edge edge) throws UnificationFailedException {
		if (scopeclosing != edge.scopeclosing) throw new UnificationFailedException();
		if (body.length != edge.body.length) throw new UnificationFailedException();
		if (progress != edge.progress) throw new UnificationFailedException();
		if (startPos != edge.startPos) throw new UnificationFailedException();
		if (endPos != edge.endPos) throw new UnificationFailedException();
		if (externalAnteList.length != edge.externalAnteList.length) throw new UnificationFailedException();
		if (internalAnteList.length != edge.internalAnteList.length) throw new UnificationFailedException();
		
		head.unify(edge.head);
		for (int i = 0 ; i < externalAnteList.length ; i++) {
			externalAnteList[i].unify(edge.externalAnteList[i]);
		}
		for (int i = 0 ; i < internalAnteList.length ; i++) {
			internalAnteList[i].unify(edge.internalAnteList[i]);
		}
		for (int i = 0 ; i < body.length ; i++) {
			body[i].unify(edge.body[i]);
		}
	}
	
	/**
	 * Tries to unify this edge with another edge. If unification is not possible, an exception
	 * is thrown. In the case unification would be possible, the unification is not performed completely.
	 * In any case the two edges remain in an unconsistent state afterwards. Thus, this operation should
	 * be performed only on copies of objects that are not used anymore afterwards.
	 * 
	 * @param edge The edge to be unified with this edge.
	 * @throws UnificationFailedException If unification fails.
	 */
	void tryToUnify(Edge edge) throws UnificationFailedException {
		if (scopeclosing != edge.scopeclosing) throw new UnificationFailedException();
		if (body.length != edge.body.length) throw new UnificationFailedException();
		if (progress != edge.progress) throw new UnificationFailedException();
		if (startPos != edge.startPos) throw new UnificationFailedException();
		if (endPos != edge.endPos) throw new UnificationFailedException();
		if (externalAnteList.length != edge.externalAnteList.length) throw new UnificationFailedException();
		if (internalAnteList.length != edge.internalAnteList.length) throw new UnificationFailedException();
		if (externalAnteList.length != edge.externalAnteList.length) throw new UnificationFailedException();
		if (internalAnteList.length != edge.internalAnteList.length) throw new UnificationFailedException();
		
		head.tryToUnify(edge.head);
		for (int i = 0 ; i < externalAnteList.length ; i++) {
			externalAnteList[i].tryToUnify(edge.externalAnteList[i]);
		}
		for (int i = 0 ; i < internalAnteList.length ; i++) {
			internalAnteList[i].tryToUnify(edge.internalAnteList[i]);
		}
		for (int i = 0 ; i < body.length ; i++) {
			body[i].tryToUnify(edge.body[i]);
		}
	}
	
	/**
	 * This methods checks whether two edges are similar. Two edges are similar if and only if
	 * the replacement of every variable occurence by a new singleton variable (in both edges)
	 * would make them equal (apart from variable names). Edges that are not similar can never
	 * unify. Similar edges may or may not unify.
	 * 
	 * @param edge The edge for which similarity with this edge should be checked.
	 * @return true if the two edges are similar.
	 */
	boolean isSimilar(Edge edge) {
		if (scopeclosing != edge.scopeclosing) return false;
		if (startPos != edge.startPos) return false;
		if (endPos != edge.endPos) return false;
		if (!head.isSimilar(edge.head)) return false;
		if (body.length != edge.body.length) return false;
		if (progress != edge.progress) return false;
		if (externalAnteList.length != edge.externalAnteList.length) return false;
		if (internalAnteList.length != edge.internalAnteList.length) return false;
		for (int i = 0 ; i < externalAnteList.length ; i++) {
			if (!externalAnteList[i].isSimilar(edge.externalAnteList[i])) return false;
		}
		for (int i = 0 ; i < internalAnteList.length ; i++) {
			if (!internalAnteList[i].isSimilar(edge.internalAnteList[i])) return false;
		}
		for (int i = 0 ; i < body.length ; i++) {
			if (!body[i].isSimilar(edge.body[i])) return false;
		}
		return true;
	}
	
	/**
	 * This method returns true if this edge subsumes (i.e. is more general than) the given edge,
	 * or false otherwise.
	 * 
	 * @param edge The edge for which it is checked whether this edge subsumes it.
	 * @return true if this edge subsumes the given edge.
	 */
	boolean subsumes(Edge edge) {
		if (!isSimilar(edge)) return false;
		
		// Both edges are copied to keep the existing edges untouched:
		Edge edge1C = deepCopy();
		Edge edge2C = edge.deepCopy();
		
		// Edge 1 subsumes edge 2 iff 1 unifies with 2 after the skolemization of 2.
		edge2C.skolemize();
		try {
			edge1C.tryToUnify(edge2C);
			return true;
		} catch (UnificationFailedException ex) {
			return false;
		}
	}
	
	/**
	 * Skolemizes the variables of this edge.
	 */
	private void skolemize() {
		head.skolemize();
		for (Category c : externalAnteList) c.skolemize();
		for (Category c : internalAnteList) c.skolemize();
		for (Category c : body) c.skolemize();
	}
	
	/**
	 * Creates a deep copy of this edge. This means that all categories are copied. The child edges
	 * can be linked or copied.
	 * 
	 * @param copyChildren defines whether child edges should be copied or just linked.
	 * @return A deep copy.
	 */
	Edge deepCopy(boolean copyChildren) {
		return deepCopy(new HashMap<Integer, StringObject>(), copyChildren);
	}
	
	/**
	 * Creates a deep copy of this edge. All categories of this edge are copied, but the child
	 * edges are linked without being copied.
	 * 
	 * @return A deep copy.
	 */
	Edge deepCopy() {
		return deepCopy(false);
	}
	
	/**
	 * Creates a deep copy of this edge using the given string objects. This method is usually
	 * called form another deepCopy-method. The child edges can be linked or copied.
	 * 
	 * @param stringObjs The string objects to be used.
	 * @param copyChildren defines whether child edges should be copied or just linked.
	 * @return A deep copy.
	 */
	Edge deepCopy(HashMap<Integer, StringObject> stringObjs, boolean copyChildren) {
		Edge edgeC = new Edge();
		edgeC.startPos = startPos;
		edgeC.endPos = endPos;
		edgeC.progress = progress;
		edgeC.scopeclosing = scopeclosing;
		edgeC.head = head.deepCopy(stringObjs);
		edgeC.body = new Category[body.length];
		for (int i=0 ; i < body.length ; i++) {
			edgeC.body[i] = body[i].deepCopy(stringObjs);
		}
		edgeC.externalAnteList = new Category[externalAnteList.length];
		for (int i=0 ; i < externalAnteList.length ; i++) {
			edgeC.externalAnteList[i] = externalAnteList[i].deepCopy(stringObjs);
		}
		edgeC.internalAnteList = new Category[internalAnteList.length];
		for (int i=0 ; i < internalAnteList.length ; i++) {
			edgeC.internalAnteList[i] = internalAnteList[i].deepCopy(stringObjs);
		}
		edgeC.annotation = annotation.deepCopy(stringObjs);
		if (copyChildren) {
			edgeC.links = new ArrayList<Edge>();
			for (Edge l : links) {
				if (l == null) {
					edgeC.links.add(null);
				} else {
					edgeC.links.add(l.deepCopy(true));
				}
			}
		} else {
			edgeC.links = new ArrayList<Edge>(links);
		}
		return edgeC;
	}
	
	/**
	 * Creates a deep copy of this edge using the given string objects. This method is usually
	 * called form another deepCopy-method. All categories of this edge are copied, but the child
	 * edges are linked without being copied.
	 * 
	 * @param stringObjs The string objects to be used.
	 * @return A deep copy.
	 */
	Edge deepCopy(HashMap<Integer, StringObject> stringObjs) {
		return deepCopy(stringObjs, false);
	}
	
	void calculateIdentifier(String[] usedFeatureNames) {
		List<Integer> vars = new ArrayList<Integer>();
		List<Integer> mvars = new ArrayList<Integer>();
		
		head.collectVars(vars, mvars);
		for (Category c : externalAnteList) {
			c.collectVars(vars, mvars);
		}
		for (Category c : internalAnteList) {
			c.collectVars(vars, mvars);
		}
		for (Category c : body) {
			c.collectVars(vars, mvars);
		}

		identifier = startPos + "," + endPos + " ";
		identifier += head.getIdentifier(mvars, usedFeatureNames) + " ";
		for (Category c : externalAnteList) {
			identifier += c.getIdentifier(mvars, usedFeatureNames);
		}
		if (scopeclosing) {
			identifier += "~";
		} else {
			identifier += "=";
		}
		for (Category c : internalAnteList) {
			identifier += c.getIdentifier(mvars, usedFeatureNames);
		}
		identifier += " " + progress;
		for (Category c : body) {
			identifier += c.getIdentifier(mvars, usedFeatureNames) + " ";
		}
	}
	
	/**
	 * Returns an identifier for this edge. Two edges have the same identifier if and only if they
	 * are equivalent.
	 * 
	 * @return The identifier of this edge.
	 */
	public String getIdentifier() {
		return identifier;
	}
	
	public boolean equals(Object obj) {
		if (identifier == null) return this == obj;
		if (!(obj instanceof Edge)) return false;
		Edge other = (Edge) obj;
		return identifier.equals(other.identifier);
	}
	
	public int hashCode() {
		if (identifier == null) return super.hashCode();
		return identifier.hashCode();
	}
	
	public String toString() {
		String s = "<" + startPos + "," + endPos + "> " + head;
		s += " ";
		for (Category f : externalAnteList) {
			s += f;
		}
		if (scopeclosing) {
			s += "~";
		} else {
			s += "=";
		}
		for (Category f : internalAnteList) {
			s += f;
		}
		s += ">";
		for (int i=0 ; i < body.length ; i++) {
			if (i == progress) s += " .";
			s += " " + body[i];
		}
		if (!isActive()) s += " .";
		return s;
	}

}
