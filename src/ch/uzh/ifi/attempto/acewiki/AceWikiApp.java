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

package ch.uzh.ifi.attempto.acewiki;

import java.util.Map;

import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.app.ContentPane;
import nextapp.echo2.app.Window;
import nextapp.echo2.webcontainer.ContainerContext;
import ch.uzh.ifi.attempto.acewiki.gui.page.LoginScreen;
import ch.uzh.ifi.attempto.echocomp.ServerDelayMessage;
import ch.uzh.ifi.attempto.echocomp.Style;

/**
 * This non-public class represents an AceWiki application instance.
 * 
 * @author Tobias Kuhn
 */
class AceWikiApp extends ApplicationInstance {
	
	private static final long serialVersionUID = -2038165609406790355L;
	
	/**
	 * No login required option.
	 */
	public static final int NO_LOGIN = 0;
	
	/**
	 * Login without password required.
	 */
	public static final int NONPW_LOGIN = 1;
	
	/**
	 * Login with password required (not fully implemented).
	 */
	public static final int PW_LOGIN = 2;

	private static int sessionID = 1;

	private Map<String, String> parameters;
	private Wiki wiki;
	private Window window;
	
	/**
	 * Creates a new AceWiki application instance.
	 * 
	 * @param parameters A set of parameters in the form of name/value pairs.
	 */
	public AceWikiApp(Map<String, String> parameters) {
		this.parameters = parameters;
	}
	
	public Window init() {
		setStyleSheet(Style.styleSheet);
		window = new Window();
		wiki = new Wiki(parameters, sessionID++);
		wiki.log("syst", "start session");
		
		// Show login window if required:
		String l = parameters.get("login");
		if ("yes".equals(l) || "pw".equals(l)) {
			ContentPane contentPane = new ContentPane();
			contentPane.add(new LoginScreen(wiki, window, parameters.get("title"), "pw".equals(l)));
			window.setContent(contentPane);
		} else {
			window.setContent(wiki.getContentPane());
		}
		
		window.setTitle("AceWiki");
		
		ContainerContext cc = (ContainerContext) ApplicationInstance.getActive()
			.getContextProperty(ContainerContext.CONTEXT_PROPERTY_NAME);
		cc.setServerDelayMessage(new ServerDelayMessage("Please wait...", "../wait.gif"));
		// (The wait icon should be copied at the right place on the server.)
		
		return window;
	}
	
	/**
	 * Logs out the current user. Note that login/logout is only implemented very rudimentary at the
	 * moment.
	 */
	public void logout() {
		String l = parameters.get("login");
		if ("yes".equals(l) || "pw".equals(l)) {
			wiki.log("syst", "logout");
			wiki = new Wiki(parameters, sessionID++);
			wiki.log("syst", "start session");
			ContentPane contentPane = new ContentPane();
			contentPane.add(new LoginScreen(wiki, window, parameters.get("title"), "pw".equals(l)));
			window.setContent(contentPane);
		}
	}
	
	/**
	 * Returns the wiki instance.
	 * 
	 * @return The wiki instance.
	 */
	public Wiki getWiki() {
		return wiki;
	}

}