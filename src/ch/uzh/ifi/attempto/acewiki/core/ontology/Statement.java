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

package ch.uzh.ifi.attempto.acewiki.core.ontology;

/**
 * This class represents a statement that can be either an ACE sentence or a comment. A
 * statement can either have an ontology element as its owner (in this case it occurs on
 * the article of the owner) or it can be an independent statement that has no owner.
 * 
 * @author Tobias Kuhn
 */
public abstract class Statement {
	
	private Ontology ontology;
	private OntologyElement owner;
	
	/**
	 * Initializes a new independent statement.
	 * 
	 * @param ontology The ontology of the new statement.
	 */
	protected Statement(Ontology ontology) {
		this.ontology = ontology;
		
	}
	
	/**
	 * Initializes a new statement with the given ontology element as its owner.
	 * 
	 * @param owner The ontology element that is the owner of the new statement.
	 */
	protected Statement(OntologyElement owner) {
		this.owner = owner;
		this.ontology = owner.getOntology();
	}
	
	/**
	 * This method returns the ontology this statement belongs to.
	 * 
	 * @return The ontology.
	 */
	public Ontology getOntology() {
		if (ontology == null) {
			ontology = owner.getOntology();
		}
		return ontology;
	}
	
	/**
	 * This method returns the owner ontology element of this statement.
	 * 
	 * @return The owner ontology element.
	 */
	public OntologyElement getOwner() {
		return owner;
	}
	
	/**
	 * This method returns the text of this statement.
	 * 
	 * @return The text.
	 */
	public abstract String getText();
	
	/**
	 * This method returns a string that denotes the type of this statement.
	 * 
	 * @return The type of this statement.
	 */
	public abstract String getType();
	
	/**
	 * This method returns a serialization of the statement.
	 * 
	 * @return The serialized representation of the statement.
	 */
	abstract String serialize();
	
	public String toString() {
		return getText();
	}

}
