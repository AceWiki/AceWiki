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

package ch.uzh.ifi.attempto.preditor;

import nextapp.echo.app.ApplicationInstance;
import nextapp.echo.app.Window;
import nextapp.echo.webcontainer.WebContainerServlet;
import ch.uzh.ifi.attempto.echocomp.Style;

/**
 * This class is an examplary implementation of a servlet that starts a predictive editor. See the
 * <a href="{@docRoot}/src-html/ch/uzh/ifi/attempto/preditor/ExampleServlet.html">source code</a>.
 * 
 * @author Tobias Kuhn
 */
public class ExampleServlet extends WebContainerServlet {
	
	private static final long serialVersionUID = -6998969461055356964L;

	/**
	 * Creates a new servlet instance.
	 */
	public ExampleServlet() {
	}

	public ApplicationInstance newApplicationInstance() {
		
		return new ApplicationInstance() {
			
			private static final long serialVersionUID = -5640636230574254208L;

			public Window init() {
				setStyleSheet(Style.styleSheet);
				Window window = new Window();
				window.setTitle("Preditor Example Application");
				
				PreditorWindow preditor = new PreditorWindow(
						"My Predictive Editor",
						new ExampleGrammar(),
						"text"
					);
				preditor.setMenuCreator(new ExampleMenuCreator());
				preditor.setClosable(false);
				window.getContent().add(preditor);
				
				return window;
			}
			
		};
		
	}

}
