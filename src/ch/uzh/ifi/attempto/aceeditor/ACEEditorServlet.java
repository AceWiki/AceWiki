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

package ch.uzh.ifi.attempto.aceeditor;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import nextapp.echo.app.ApplicationInstance;
import nextapp.echo.app.Window;
import nextapp.echo.webcontainer.WebContainerServlet;
import ch.uzh.ifi.attempto.ape.APELocal;
import ch.uzh.ifi.attempto.echocomp.Style;

/**
 * This servlet class is used by the web server to start the ACE Editor web application.
 * 
 * @author Tobias Kuhn
 */
public class ACEEditorServlet extends WebContainerServlet {

	private static final long serialVersionUID = -2533205689651186115L;

	/**
	 * Creates a new servlet for the ACE Editor web application.
	 */
	public ACEEditorServlet() {
	}

	public ApplicationInstance newApplicationInstance() {

		return new ApplicationInstance() {

			private static final long serialVersionUID = 2982410120358060245L;

			public Window init() {
				setStyleSheet(Style.styleSheet);
				
				if (!APELocal.isInitialized()) {
					String apeCommand = getServletContext().getInitParameter("apecommand");
					if (apeCommand == null) apeCommand = "ape.exe";
					APELocal.init(apeCommand);
				}
				
				return new ACEEditor(getInitParameters());
			}

		};

	}

	@SuppressWarnings("unchecked")
	private Map<String, String> getInitParameters() {
		Map<String, String> initParameters = new HashMap<String, String>();
		Enumeration paramEnum = getInitParameterNames();
		while (paramEnum.hasMoreElements()) {
			String paramName = paramEnum.nextElement().toString();
			initParameters.put(paramName, getInitParameter(paramName));
		}
		return initParameters;
	}

}
