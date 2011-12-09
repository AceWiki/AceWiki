// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * This class stands for the set of registered users for a particular AceWiki instance.
 * 
 * @author Tobias Kuhn
 */
public class UserBase {
	
	private Ontology ontology;
	private AceWikiStorage storage;
	private long idCount = 0;
	private Map<String, User> userNameMap = new HashMap<String, User>();
	private Map<Long, User> userIdMap = new HashMap<Long, User>();
	
	/**
	 * Creates a new user base for the given ontology.
	 * 
	 * @param ontology The ontology.
	 */
	UserBase(Ontology ontology, AceWikiStorage storage) {
		this.ontology = ontology;
		this.storage = storage;
	}
	
	/**
	 * Adds a user to this user base.
	 * 
	 * @param user The user to be added.
	 */
	void addUser(User user) {
		if (user == null) return;
		if (user.getId() > idCount) idCount = user.getId();
		userNameMap.put(user.getName(), user);
		userIdMap.put(user.getId(), user);
	}
	
	/**
	 * Checks whether a user with the respective name exists.
	 * 
	 * @param name The user name.
	 * @return true if the user exists.
	 */
	public boolean containsUser(String name) {
		return userNameMap.containsKey(name);
	}
	
	/**
	 * Returns the user with the given id, or null if no user with this id exists.
	 * 
	 * @param id The user id.
	 * @return The user object.
	 */
	public User getUser(long id) {
		return userIdMap.get(id);
	}
	
	/**
	 * Returns the number of registered users.
	 * 
	 * @return The number of users.
	 */
	public int getUserCount() {
		return userIdMap.size();
	}
	
	/**
	 * Tries to login a user. The user object is returned if the login was successful. Null is
	 * returned otherwise.
	 * 
	 * @param name The name of the user.
	 * @param password The password in plain text.
	 * @return The user object.
	 */
	public User login(String name, String password) {
		User user = userNameMap.get(name);
		if (user == null) return null;
		if (user.isCorrectPassword(password)) {
			user.setUserData("lastlogin", getTimeNow());
			user.addToUserDataCounter("logincount", 1);
			return user;
		}
		return null;
	}
	
	/**
	 * Tries to do an auto-login. The user object is returned if the login was successful. Null is
	 * returned otherwise.
	 * 
	 * @param name The name of the user.
	 * @param clientToken The auto-login-token from the client browser.
	 * @return The user object.
	 */
	public User autoLogin(String name, String clientToken) {
		User user = userNameMap.get(name);
		if (user == null) return null;
		String serverToken = user.getUserData("stayloggedintoken");
		if (clientToken.equals(serverToken)) {
			user.setUserData("lastlogin", getTimeNow());
			user.addToUserDataCounter("logincount", 1);
			return user;
		}
		return null;
	}
	
	/**
	 * Registers a new user. The new user object is returned if the registration was successful.
	 * Null is returned otherwise.
	 * 
	 * @param name The name of the new user.
	 * @param email The email address of the new user.
	 * @param password The password for the new user.
	 * @return The new user object.
	 */
	public User register(String name, String email, String password) {
		if (userNameMap.get(name) != null) return null;
		Map<String,String> userdata = new HashMap<String, String>();
		userdata.put("email", email);
		String now = getTimeNow();
		userdata.put("registerdate", now);
		userdata.put("lastlogin", now);
		userdata.put("logincount", "1");
		long id = ++idCount;
		User user = User.createUser(id, name, password, userdata, this);
		addUser(user);
		return user;
	}
	
	/**
	 * Returns the ontology object.
	 * 
	 * @return The ontology.
	 */
	public Ontology getOntology() {
		return ontology;
	}
	
	/**
	 * Returns the storage object.
	 * 
	 * @return The storage.
	 */
	public AceWikiStorage getStorage() {
		return storage;
	}
	
	private String getTimeNow() {
		return (new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")).format(new Date());
	}

}
