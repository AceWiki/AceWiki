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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * This class represents a chart (in the sense of a chart for a chart parser). A chart basically consists
 * of a set of edges plus operations to add, remove, and retrieve edges. The edges stored by a chart represent
 * (partial) analyses of certain ranges of a text.
 * 
 * @author Tobias Kuhn
 */
class Chart {
	
	private String[] usedFeatureNames;
	
	// All edges are contained by this set:
	private Set<Edge> edges = new HashSet<Edge>();
	
	// This hashtable indexes the edges by their end positions:
	private Map<Integer, List<Edge>> edgesByEndPos = new HashMap<Integer, List<Edge>>();
	
	/**
	 * Creates an empty chart.
	 * 
	 * @param grammar The grammar for the chart.
	 */
	public Chart(Grammar grammar) {
		this.usedFeatureNames = grammar.getFeatureNamesArray();
	}
	
	/**
	 * Adds the given edge to the chart. If there is already an edge in the chart that is equivalent
	 * to the given edge then the chart remains unchanged and false is returned.
	 * 
	 * @param edge The edge to be added to the chart.
	 * @return true if the edge was has been to the chart, or false if there is already an
	 *     equivalent edge in the chart.
	 */
	public boolean addEdge(Edge edge) {
		// Check whether the edge is new (i.e. not equivalent to an existing edge):
		edge.calculateIdentifier(usedFeatureNames);
		boolean isNewEdge = !edges.contains(edge);
		
		if (isNewEdge) {
			// Add the edge to the chart:
			edges.add(edge);
			
			// Update the end position hashtable:
			getEdgesByEndPos(edge.getEndPos()).add(edge);
		}
		
		return isNewEdge;
	}
	
	/**
	 * Returns the size of the chart, i.e. the number of edges.
	 * 
	 * @return The size of the chart.
	 */
	public int getSize() {
		return edges.size();
	}
	
	/**
	 * Returns a list of all edges with the given end position. The returned list in an internal list
	 * that should never be changed from outside.
	 * 
	 * @param endPos The end position of the edges to be returned.
	 * @return A list of all edges with the given end position.
	 */
	public List<Edge> getEdgesByEndPos(int endPos) {
		List<Edge> l = edgesByEndPos.get(endPos);
		if (l == null) {
			l = new ArrayList<Edge>();
			edgesByEndPos.put(endPos, l);
		}
		return l;
	}
	
	/**
	 * Removes all edges with the given end position from the chart.
	 * 
	 * @param endPos The end position of the edges to be removed.
	 */
	public void removeEdgesWithEndPos(int endPos) {
		List<Edge> l = edgesByEndPos.get(endPos);
		if (l == null) return;
		for (Edge e : l) {
			edges.remove(e);
		}
		l.clear();
	}
	
	/**
	 * Removes all edges from the chart.
	 */
	public void clear() {
		edges.clear();
		edgesByEndPos.clear();
	}
	
	public String toString() {
		String s = "";
		for (Edge e : edges) {
			s += e + "\n";
		}
		return s;
	}

}
