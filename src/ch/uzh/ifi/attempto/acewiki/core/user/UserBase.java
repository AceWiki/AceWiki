package ch.uzh.ifi.attempto.acewiki.core.user;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import ch.uzh.ifi.attempto.acewiki.core.ontology.Ontology;

/**
 * This class stands for the set of registered users for a particular AceWiki instance.
 * 
 * @author Tobias Kuhn
 */
public class UserBase {
	
	private static Map<String, UserBase> userBaseMap = new HashMap<String, UserBase>();
	
	private Ontology ontology;
	private long idCount = 0;
	private Map<String, User> userNameMap = new HashMap<String, User>();
	private Map<Long, User> userIdMap = new HashMap<Long, User>();
	
	/**
	 * Returns the user base for the given ontology.
	 * 
	 * @param ontology The ontology.
	 * @return The user base.
	 */
	public static UserBase getUserBase(Ontology ontology) {
		UserBase userBase = userBaseMap.get(ontology.getName());
		if (userBase != null) return userBase;
		return new UserBase(ontology);
	}
	
	/**
	 * Creates a new user base for the given ontology.
	 * 
	 * @param ontology The ontology.
	 */
	private UserBase(Ontology ontology) {
		this.ontology = ontology;
		String dataDirPath = "data/";
		String userDirPath = dataDirPath + ontology.getName() + ".users";
		File dataDir = new File(dataDirPath);
		File userDir = new File(userDirPath);
		if (!dataDir.exists()) dataDir.mkdir();
		if (userDir.exists()) {
			for (File file : userDir.listFiles()) {
				User user = User.loadUser(file);
				if (user.getId() > idCount) idCount = user.getId();
				addUser(user);
			}
		} else {
			userDir.mkdir();
		}
	}
	
	/**
	 * Adds a user to this user base.
	 * 
	 * @param user The user to be added.
	 */
	private void addUser(User user) {
		if (user == null) return;
		userNameMap.put(user.getName(), user);
		userIdMap.put(user.getId(), user);
	}
	
	/**
	 * Returns the user with the respective name, or null if no such user exists.
	 * 
	 * @param name The name of the user.
	 * @return The user object.
	 */
	public User getUser(String name) {
		return userNameMap.get(name);
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
		User user = getUser(name);
		if (user == null) return null;
		if (user.isCorrectPassword(password)) {
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
		if (getUser(name) != null) return null;
		Map<String,String> userdata = new HashMap<String, String>();
		userdata.put("email", email);
		String now = getTimeNow();
		userdata.put("registerdate", now);
		userdata.put("lastlogin", now);
		userdata.put("logincount", "1");
		long id = ++idCount;
		User user = User.createUser(
				id,
				name,
				password,
				userdata,
				new File("data/" + ontology.getName() + ".users" + "/" + id)
			);
		addUser(user);
		return user;
	}
	
	private String getTimeNow() {
		return (new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")).format(new Date());
	}

}
