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

package ch.uzh.ifi.attempto.acewiki.core;

import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * This class represents an AceWiki user.
 * 
 * @author Tobias Kuhn
 */
public class User {
	
	private final long id;
	private String name;
	private String hashedPw;
	private Map<String, String> userdata;
	private final UserBase userBase;
	
	/**
	 * Creates a new user.
	 * 
	 * @param id The user id.
	 * @param name The name of the user.
	 * @param hashedPw The hashed password.
	 * @param userdata The user data map.
	 */
	User(long id, String name, String hashedPw, Map<String, String> userdata, UserBase userBase) {
		this.id = id;
		this.name = name;
		this.hashedPw = hashedPw;
		this.userdata = userdata;
		this.userBase = userBase;
		save();
	}
	
	/**
	 * Creates a new user.
	 * 
	 * @param id The user id.
	 * @param name The name of the user.
	 * @param pw The password in plain text.
	 * @param userdata The user data map.
	 * @return The new user object.
	 */
	static User createUser(long id, String name, String pw, Map<String, String> userdata, UserBase userBase) {
		return new User(id, name, getPasswordHash(pw), userdata, userBase);
	}
	
	/**
	 * Returns the id of the user.
	 * 
	 * @return The id.
	 */
	public long getId() {
		return id;
	}
	
	/**
	 * Returns the name of the user.
	 * 
	 * @return The name of the user.
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Returns the hashed password.
	 * 
	 * @return The hashed password.
	 */
	public String getHashedPassword() {
		return hashedPw;
	}
	
	/**
	 * Returns the user data with the given property name.
	 * 
	 * @param name The property name.
	 * @return The data for the respective name.
	 */
	public String getUserData(String name) {
		String value = userdata.get(name);
		if (value == null) return "";
		return value;
	}
	
	/**
	 * Sets the user data element with the respective name.
	 * 
	 * @param name The name of the user data element.
	 * @param value The value to be set.
	 */
	public void setUserData(String name, String value) {
		userdata.put(name, value);
		save();
	}
	
	/**
	 * Changes the password for the user.
	 * 
	 * @param oldPw The old password in plain text.
	 * @param newPw The new password in plain text.
	 */
	public void changePassword(String oldPw, String newPw) {
		if (!isCorrectPassword(oldPw)) return;
		hashedPw = getPasswordHash(newPw);
		save();
	}
	
	/**
	 * Adds to a counter in the user data.
	 * 
	 * @param name The name of the user data element.
	 * @param c The value by which the counter should be increased.
	 */
	public void addToUserDataCounter(String name, int c) {
		String value = userdata.get(name);
		if (value == null) {
			userdata.put(name, c + "");
		} else {
			int v;
			try {
				v = new Integer(value);
			} catch (NumberFormatException ex) {
				v = 0;
			}
			userdata.put(name, (v + c) + "");
		}
		save();
	}
	
	/**
	 * Checks whether a certain password is the correct password for this user.
	 * 
	 * @param pw The password to be checked in plain text.
	 * @return true if the password is correct.
	 */
	public boolean isCorrectPassword(String pw) {
		return getPasswordHash(pw).equals(hashedPw);
	}
	
	/**
	 * Returns a hash value for a given plain-text password using the SHA-256 algorithm.
	 * 
	 * @param password The plain-text password for which a hash value should be created.
	 * @return The hash value.
	 */
	public static String getPasswordHash(String password) {
		try {
			MessageDigest md = MessageDigest.getInstance("SHA-256");
			md.update(password.getBytes());
			byte[] byteData = md.digest();
			StringBuffer sb = new StringBuffer();
			for (int i = 0; i < byteData.length; i++) {
				sb.append(Integer.toString((byteData[i] & 0xff) + 0x100, 16).substring(1));
			}
			return sb.toString();
		} catch (Exception ex) {
			ex.printStackTrace();
			return null;
		}
	}
	
	/**
	 * Returns the user base to which this user belongs.
	 * 
	 * @return The user base.
	 */
	public UserBase getUserBase() {
		return userBase;
	}
	
	/**
	 * Returns all key of this user's properties in the form of key/value pairs.
	 * 
	 * @return All user data keys.
	 */
	public List<String> getUserDataKeys() {
		return new ArrayList<String>(userdata.keySet());
	}
	
	private void save() {
		userBase.getStorage().save(this);
	}

}
