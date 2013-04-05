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

package ch.uzh.ifi.attempto.base;

/**
 * This class is used to save the necessary log context, information.
 * 
 * @author Victor Ungureanu
 */
public class LoggerContext {
	private final String module;
	private String username;
	private final String sessionId;
	
	/**
	 * Creates a new logger context for the given module/ontology, user name, and session id.
	 * 
	 * @param module The module or ontology name
	 * @param username The user name.
	 * @param sessionId The session id.
	 */
	public LoggerContext(String module, String username, String sessionId) {
		this.module = module;
		this.username = username;
		this.sessionId = sessionId;
	}

	/**
	 * Sets the user name.
	 * 
	 * @param username The user name.
	 */
	public void setUsername(String username) {
		this.username = username;
	}
	
	/**
	 * Propagates this logger context within the current thread, using SLF4J API.
	 * As such, calling this method has (only) side effects.
	 */
	public void propagateWithinThread() {
		org.slf4j.MDC.put("module", module);
		org.slf4j.MDC.put("username", username);
		org.slf4j.MDC.put("sessionId", sessionId);
	}
}
