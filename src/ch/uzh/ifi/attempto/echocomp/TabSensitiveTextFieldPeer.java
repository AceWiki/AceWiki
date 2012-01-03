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

package ch.uzh.ifi.attempto.echocomp;

import nextapp.echo.app.Component;
import nextapp.echo.app.util.Context;

import nextapp.echo.webcontainer.ServerMessage;
import nextapp.echo.webcontainer.WebContainerServlet;
import nextapp.echo.webcontainer.service.JavaScriptService;

import nextapp.echo.webcontainer.sync.component.TextFieldPeer;

/**
 * This is the peer component for the class {@link TabSensitiveTextField}.
 * 
 * @author Tobias Kuhn
 */
public class TabSensitiveTextFieldPeer extends TextFieldPeer {

	private static final String REGISTRY_KEY = "Attempto.TabSensitiveTextField";
	private static final String JAVASCRIPT_PATH = "ch/uzh/ifi/attempto/echocomp/TabSensitiveTextField.js";
	
	static {
		WebContainerServlet.getServiceRegistry().add(
				JavaScriptService.forResource(REGISTRY_KEY, JAVASCRIPT_PATH)
			);
	}
	
	public void init(Context context, Component component) {
		super.init(context, component);
		
		ServerMessage serverMessage = (ServerMessage) context.get(ServerMessage.class);
		serverMessage.addLibrary(REGISTRY_KEY);
	}
	
	@SuppressWarnings("rawtypes")
	public Class getComponentClass() {
		return TabSensitiveTextField.class;
	}
	
	public String getClientComponentType(boolean shortType) {
		return REGISTRY_KEY;
	}

}
