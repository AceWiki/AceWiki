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

package ch.uzh.ifi.attempto.acewiki.core;

/**
 * This interface represents a grammar module, i.e. a technical meta element.
 * 
 * @author Tobias Kuhn
 */
public interface ModuleElement extends MetaOntologyElement {

	public void integrate();

	public void parse() throws InvalidSyntaxException;

	// TODO Content of a module is a Comment object; should be changed
	public Comment getModuleContent();

	public void replaceModuleContent(String newContent);

	public String getDefaultContent();


	public class InvalidSyntaxException extends Exception {

		private static final long serialVersionUID = -6183195084383311172L;

		private String text;
		private Integer line, column;

		public InvalidSyntaxException(String text, Integer line, Integer column) {
			this.text = text;
			this.line = line;
			this.column = column;
		}

		public String getText() {
			return text;
		}

		public Integer getLine() {
			return line;
		}

		public Integer getColumn() {
			return column;
		}

	}

}
