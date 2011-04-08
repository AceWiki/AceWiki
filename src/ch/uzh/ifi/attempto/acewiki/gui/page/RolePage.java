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

package ch.uzh.ifi.attempto.acewiki.gui.page;

import nextapp.echo.app.event.ActionEvent;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Role;

/**
 * This class stands for an article page showing the article of a role. Roles are represented by
 * verbs, of-constructs, or transitive adjectives.
 * 
 * @author Tobias Kuhn
 */
public class RolePage extends ArticlePage {
	
	private static final long serialVersionUID = -7034483028750537141L;
	
	private Role role;
	
	/**
	 * Creates a new article page for a role.
	 * 
	 * @param role The role.
	 * @param wiki The wiki instance.
	 */
	public RolePage(Role role, Wiki wiki) {
		super(wiki, role);
		this.role = role;
	}
	
	public OntologyElement getOntologyElement() {
		return role;
	}

	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
	}
	
	protected void doUpdate() {
		super.doUpdate();
		
		getTitle().setText(role.getHeadword());
	}

}
