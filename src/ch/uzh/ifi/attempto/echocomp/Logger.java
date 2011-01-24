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

package ch.uzh.ifi.attempto.echocomp;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Calendar;

/**
 * This class is used to log the events on the server.
 * 
 * @author Tobias Kuhn
 */
public class Logger {
	
	private final String fileName;
	private String username;
	private final int sessionID;
	
	/**
	 * Creates a new logger instance for the given file, user name, and session id.
	 * 
	 * @param fileName The name of the log file.
	 * @param username The user name.
	 * @param sessionID The session id.
	 */
	public Logger(String fileName, String username, int sessionID) {
		this.fileName = fileName;
		this.username = username;
		this.sessionID = sessionID;
	}
	
	/**
	 * Creates a new logger instance for the given file and session id.
	 * 
	 * @param fileName The name of the log file.
	 * @param sessionID The session id.
	 */
	public Logger(String fileName, int sessionID) {
		this(fileName, "", sessionID);
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
	 * Writes a log entry into the log file of the respective ontology.
	 * 
	 * @param type The type of the log entry.
	 * @param text The text of the log entry.
	 */
	public void log(String type, String text) {
		log(fileName, username, sessionID, type, text);
	}
	
	/**
	 * Writes a log entry into the specified log file.
	 * 
	 * @param fileName The name of the log file.
	 * @param userName The user name.
	 * @param sessionID The session id.
	 * @param type The type of the log entry.
	 * @param text The text of the log entry.
	 */
	public static void log(String fileName, String userName, int sessionID, String type, String text) {
		Calendar c = Calendar.getInstance();
		long timestamp = System.currentTimeMillis();
		c.setTimeInMillis(timestamp);
		String year = c.get(Calendar.YEAR) + "";
		String month = makeString(c.get(Calendar.MONTH)+1, 2);
		String day = makeString(c.get(Calendar.DAY_OF_MONTH), 2);
		String hour = makeString(c.get(Calendar.HOUR_OF_DAY), 2);
		String min = makeString(c.get(Calendar.MINUTE), 2);
		String sec = makeString(c.get(Calendar.SECOND), 2);
		String millis = makeString(c.get(Calendar.MILLISECOND), 3);
		String dateTime = year + "-" + month + "-" + day + " " + hour + ":" + min + ":" + sec + "." + millis;
		String session = makeString(sessionID, 4);
		if (userName == null || userName.equals("")) {
			userName = "";
		} else {
			userName = " '" + userName + "'";
		}
		try {
			if (!(new File("logs")).exists()) (new File("logs")).mkdir();
			DataOutputStream out = new DataOutputStream(new FileOutputStream("logs/" + fileName + ".log", true));
			out.writeBytes(timestamp + " (" + dateTime + ") [" + session + "]" + userName + " [" + type + "] " + text.replaceAll("\\n", "~n") + "\n");
			out.flush();
			out.close();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}
	
	private static String makeString(int value, int size) {
		String s = value + "";
		while (s.length() < size) {
			s = "0" + s;
		}
		return s;
	}

}
