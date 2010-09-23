package ch.uzh.ifi.attempto.acewiki.core.user;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import ch.uzh.ifi.attempto.acewiki.core.ontology.Ontology;
import ch.uzh.ifi.attempto.echocomp.Logger;

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
		File dataDir = new File("data/" + ontology.getName() + ".users");
		if (dataDir.exists()) {
			for (File file : dataDir.listFiles()) {
				try {
					long id = new Long(file.getName());
					if (id > idCount) idCount = id;
					FileInputStream in = new FileInputStream(file);
					byte[] bytes = new byte[in.available()];
					in.read(bytes);
					in.close();
					String s = new String(bytes, "UTF-8");
					User user = User.loadUser(id, s);
					register(user);
				} catch (NumberFormatException ex) {
					log("ignoring file: " + file.getName());
				} catch (IOException ex) {
					log("cannot read file: " + file.getName());
				}
			}
		}
	}
	
	/**
	 * Registers a new user.
	 * 
	 * @param user A new user.
	 */
	private void register(User user) {
		if (user == null) return;
		userNameMap.put(user.getName(), user);
		userIdMap.put(user.getId(), user);
		save(user);
	}
	
	/**
	 * Saves the data of the given user into a file on the server.
	 * 
	 * @param user The user whose data should be stored.
	 */
	private void save(User user) {
		String dataDirPath = "data/";
		String userDirPath = dataDirPath + ontology.getName() + ".users";
		File dataDir = new File(dataDirPath);
		File userDir = new File(userDirPath);
		if (!dataDir.exists()) dataDir.mkdir();
		if (!userDir.exists()) userDir.mkdir();
		
		try {
			FileOutputStream out = new FileOutputStream(userDirPath + "/" + user.getId());
			out.write(user.serialize().getBytes("UTF-8"));
			out.close();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
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
		if (user.isCorrectPassword(password)) return user;
		return null;
	}
	
	/**
	 * Registers a new user. The new user object is returned if the registration was successful.
	 * Null is returned otherwise.
	 * 
	 * @param name The name of the new user.
	 * @param password The password for the new user.
	 * @return The new user object.
	 */
	public User register(String name, String password) {
		if (getUser(name) != null) return null;
		User user = User.createUser(++idCount, name, password);
		register(user);
		return user;
	}
	
	private void log(String text) {
		Logger.log(ontology.getName(), "onto", 0, "onto", text);
	}

}
