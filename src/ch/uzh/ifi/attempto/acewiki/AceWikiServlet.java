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

import java.io.IOException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.webcontainer.WebContainerServlet;
import ch.uzh.ifi.attempto.ape.APELocal;
import ch.uzh.ifi.attempto.echocomp.Logger;

/**
 * This servlet class is used by the web server to start AceWiki.
 * In order to run the AceWiki servlet, a web application archive (WAR) file has to be created.
 * See the <a href="{@docRoot}/README.txt">README file of the Attempto Java Packages</a> and the
 * <a href="{@docRoot}/web.xml">web.xml example file</a>.
 *<p>
 * SWI Prolog needs to be installed on the server and you need to have a compiled version of the
 * Attempto APE distribution. See the documentation of {@link APELocal} for more information.
 *<p>
 * For larger ontologies it might be necessary to adjust the stack and heap size, for example by
 * the following Java VM arguments:
 * <code>-Xmx400m -Xss4m</code>
 * 
 * @author Tobias Kuhn
 */
public class AceWikiServlet extends WebContainerServlet {

	private static final long serialVersionUID = -7342857942059126499L;

	/**
	 * Creates a new AceWiki servlet object.
	 */
	public AceWikiServlet() {
	}

	public ApplicationInstance newApplicationInstance() {

		if (!APELocal.isInitialized()) {
			String apeCommand = getServletContext().getInitParameter("apecommand");
			if (apeCommand == null) apeCommand = "ape.exe";
			APELocal.init(apeCommand);
		}
		
		String onto = getInitParameter("ontology");
		Logger.log("syst", "syst", 0, "appl", "new application instance: " + onto);
		
		return new AceWikiApp(getInitParameters());
	}

	protected void process(HttpServletRequest request, HttpServletResponse response) throws
			IOException, ServletException {
		
		// URLs of the form "...?showpage=ArticleName" can be used to access an article directly.
		// For the internal processing "...?page=ArticleName" is used.
		String showpageParam = request.getParameter("showpage");
		if ("".equals(showpageParam)) showpageParam = null;
		String pageParam = request.getParameter("page");
		if ("".equals(pageParam)) pageParam = null;
		String serviceidParam = request.getParameter("serviceId");
		if ("".equals(serviceidParam)) serviceidParam = null;

		if (!request.getSession().isNew() && showpageParam != null) {
			response.sendRedirect(
					response.encodeRedirectURL("?serviceId=ExternalEvent&page=" + showpageParam)
				);
		}
		if (showpageParam == null && pageParam != null && serviceidParam == null) {
			response.sendRedirect(response.encodeRedirectURL("."));
		}

		try {
			super.process(request, response);
		} catch (RuntimeException ex) {
			Logger.log("syst", "syst", 0, "fail", "fatal error: " + ex);
			ex.printStackTrace();
			throw ex;
		} catch (IOException ex) {
			Logger.log("syst", "syst", 0, "fail", "fatal error: " + ex);
			ex.printStackTrace();
			throw ex;
		} catch (ServletException ex) {
			Logger.log("syst", "syst", 0, "fail", "fatal error: " + ex);
			ex.printStackTrace();
			throw ex;
		}
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