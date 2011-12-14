// This file is part of AceWiki.
// Copyright 2011, AceWiki developers.
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

import javax.servlet.ServletException;
import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServlet;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.Reader;

import java.util.HashMap;
import java.util.Map;
import java.util.ArrayList;
import java.util.List;
import java.util.Enumeration;

import ch.uzh.ifi.attempto.base.APE;

/**
 * This class is a servlet that create a Backend object and share it to AceWiki Servlet.
 *
 * To use a Backend object from a particular BackendServlet, add "backend"
 * parameter to AceWiki servlet configure and set its value to the BackendServlet
 * name.
 * <servlet>
 *   <servlet-name>acewiki1<servlet-name>
 *   <servlet-class>ch.uzh.ifi.attempto.acewiki.AceWikiServlet</servlet-class>
 *   <load-on-startup>2</load-on-startup>
 *   <init-param>
 *     <param-name>backend</param-name>
 *     <param-value>backend1</param-value>
 *   </init-param>
 * </servlet>
 * <servlet>
 *   <servlet-name>backend1<servlet-name>
 *   <servlet-class>ch.uzh.ifi.attempto.acewiki.BackendServlet</servlet-class>
 *   <load-on-startup>1</load-on-startup>
 *   <!-- other parameter -->
 * </servlet>
 *
 * This servlet accept all parameters the original AceWiki accept.
 *
 * @author Yu Changyuan
 */
public class BackendServlet extends HttpServlet {
    private Backend backend;
    private static final long serialVersionUID = 1358039576597838L;

    @SuppressWarnings("rawtypes")
        private Map<String, String> getInitParameters(ServletConfig config) {

        Map<String, String> initParameters = new HashMap<String, String>();
        Enumeration paramEnum = config.getInitParameterNames();
        while (paramEnum.hasMoreElements()) {
            String n = paramEnum.nextElement().toString();
            initParameters.put(n, config.getInitParameter(n));
        }
        Enumeration contextParamEnum = config.getServletContext().getInitParameterNames();
        while (contextParamEnum.hasMoreElements()) {
            String n = contextParamEnum.nextElement().toString();
            initParameters.put("context:" + n, config.getServletContext().getInitParameter(n));
        }
        return initParameters;
    }

    public void init(ServletConfig config) throws ServletException {
        Map<String, String> parameters = getInitParameters(config);
        String name = config.getServletName();

        if (parameters.get("context:apecommand") == null) {
            parameters.put("context:apecommand", "ape.exe");
        }

        if (parameters.get("context:logdir") == null) {
            parameters.put("context:logdir", "logs");
        }

        if (parameters.get("context:datadir") == null) {
            parameters.put("context:datadir", "data");
        }

        APE.setParameters(parameters);

        backend = new Backend(parameters);

        ServletContext ctx = config.getServletContext();
        ctx.setAttribute(name, backend);

        super.init(config);
    }
}

