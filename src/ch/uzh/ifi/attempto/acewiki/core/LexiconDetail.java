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

public class LexiconDetail {
	
	private String name;
	private String description;
	private Object value;
	private boolean required;
	
	public LexiconDetail(String name, String description, Object value, boolean required) {
		if (value == null) value = "";
		this.name = name;
		this.description = description;
		this.value = value;
		this.required = required;
	}
	
	public LexiconDetail(String name, String description, Object value) {
		this(name, description, value, true);
	}
	
	public LexiconDetail(String name, Object value) {
		this(name, "", value, true);
	}
	
	public String getName() {
		return name;
	}
	
	public String getDescription() {
		return description;
	}
	
	public Object getValue() {
		return value;
	}
	
	public boolean isRequired() {
		return required;
	}

}
