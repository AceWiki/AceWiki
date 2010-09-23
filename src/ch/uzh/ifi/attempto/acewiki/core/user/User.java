package ch.uzh.ifi.attempto.acewiki.core.user;

import java.security.MessageDigest;

/**
 * This class represents an AceWiki user.
 * 
 * @author Tobias Kuhn
 */
public class User {
	
	private final long id;
	private String name;
	private String passwordHash;
	
	/**
	 * Creates a new user.
	 * 
	 * @param id The user id.
	 * @param name The name of the user.
	 * @param passwordHash The hashed password.
	 */
	private User(long id, String name, String passwordHash) {
		this.id = id;
		this.name = name;
		this.passwordHash = passwordHash;
	}
	
	/**
	 * Loads a user from a serialized form.
	 * 
	 * @param id The user id.
	 * @param serializedUser The serialized form of the user data.
	 * @return The new user object.
	 */
	static User loadUser(long id, String serializedUser) {
		String[] lines = serializedUser.split("\n");
		if (lines.length < 2 || !lines[0].startsWith("name:") || !lines[1].startsWith("pw:")) {
			System.err.println("Invalid user file: " + id);
			return null;
		}
		String name = lines[0].substring("name:".length());
		String pw = lines[1].substring("pw:".length());
		if (pw.startsWith("\"") && pw.endsWith("\"")) {
			pw = getHashValue(pw.substring(1, pw.length()-1));
		}
		return new User(id, name, pw);
	}
	
	/**
	 * Creates a new user.
	 * 
	 * @param id The user id.
	 * @param name The name of the user.
	 * @param password The password in plain text.
	 * @return The new user object.
	 */
	static User createUser(long id, String name, String password) {
		return new User(id, name, getHashValue(password));
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
	 * Checks whether a certain password is the correct password for this user.
	 * 
	 * @param password The password to be checked in plain text.
	 * @return true if the password is correct.
	 */
	public boolean isCorrectPassword(String password) {
		return getHashValue(password).equals(passwordHash);
	}
	
	/**
	 * Returns a serialized form of the user data, which is used to store the user data in a file
	 * on the server side.
	 * 
	 * @return The serialized user data.
	 */
	String serialize() {
		return "name:" + name + "\n" + "pw:" + passwordHash + "\n";
	}
	
	/**
	 * Returns a hash value for a given text using the SHA-256 algorithm.
	 * 
	 * @param text The text for which a hash value should be created.
	 * @return The hash value.
	 */
	private static String getHashValue(String text) {
		try {
			MessageDigest md = MessageDigest.getInstance("SHA-256");
			md.update(text.getBytes());
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

}
