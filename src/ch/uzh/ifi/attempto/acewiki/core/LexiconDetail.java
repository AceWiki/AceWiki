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

package ch.uzh.ifi.attempto.acewiki.core;

/**
 * This class represents a detail of a lexical entry as shown to the user. For example, the plural
 * form of a noun would be such a detail.
 * 
 * @author Tobias Kuhn
 */
public class LexiconDetail {
	
	private String name;
	private String description;
	private Object value;
	private boolean required;
	
	/**
	 * Creates a lexical detail object.
	 * 
	 * @param name The name of the detail as shown to the user.
	 * @param description The description shown to the user.
	 * @param value The value, either a String or a Boolean.
	 * @param required Whether the detail is required or not.
	 */
	public LexiconDetail(String name, String description, Object value, boolean required) {
		if (value == null) value = "";
		this.name = name;
		this.description = description;
		this.value = value;
		this.required = required;
	}

	/**
	 * Creates a required lexical detail object.
	 * 
	 * @param name The name of the detail as shown to the user.
	 * @param description The description shown to the user.
	 * @param value The value, either a String or a Boolean.
	 */
	public LexiconDetail(String name, String description, Object value) {
		this(name, description, value, true);
	}

	/**
	 * Creates a required lexical detail object without description.
	 * 
	 * @param name The name of the detail as shown to the user.
	 * @param value The value, either a String or a Boolean.
	 */
	public LexiconDetail(String name, Object value) {
		this(name, "", value, true);
	}
	
	/**
	 * Returns the name of this lexical detail.
	 * 
	 * @return The name.
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Returns the description of this lexical detail.
	 * 
	 * @return The description.
	 */
	public String getDescription() {
		return description;
	}
	
	/**
	 * Returns the value of this lexical detail, either a String or a Boolean.
	 * 
	 * @return The value.
	 */
	public Object getValue() {
		return value;
	}
	
	/**
	 * Returns whether this lexical detail is required or not.
	 * 
	 * @return true if this lexical detail is required.
	 */
	public boolean isRequired() {
		return required;
	}

}
