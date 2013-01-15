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

import nextapp.echo.app.ApplicationInstance;

/**
 * This class represents an Echo-aware version of a thread that knows which Echo application
 * instance it belongs to.
 * 
 * @author Tobias Kuhn
 */
public abstract class EchoThread extends Thread {
	
	/**
	 * Returns the Echo application instance of this thread.
	 * 
	 * @return The application instance.
	 */
	public abstract ApplicationInstance getApplication();
	
	/**
	 * Returns the current application instance, using Echo-aware threads if available.
	 * 
	 * @return The application instance.
	 */
	public static ApplicationInstance getActiveApplication() {
		Thread currentThread = Thread.currentThread();
		if (currentThread instanceof EchoThread) {
			return ((EchoThread) currentThread).getApplication();
		}
		return ApplicationInstance.getActive();
	}

}
