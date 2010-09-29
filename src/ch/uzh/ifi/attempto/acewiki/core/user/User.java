package ch.uzh.ifi.attempto.acewiki.core.user;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
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
	private String pwHash;
	private Map<String, String> userdata;
	private File file;
	
	/**
	 * Creates a new user.
	 * 
	 * @param id The user id.
	 * @param name The name of the user.
	 * @param pwHash The hashed password.
	 * @param userdata The user data map.
	 */
	private User(long id, String name, String pwHash, Map<String, String> userdata, File file) {
		this.id = id;
		this.name = name;
		this.pwHash = pwHash;
		this.userdata = userdata;
		this.file = file;
		save();
	}
	
	static User loadUser(File file) {
		try {
			long id = new Long(file.getName());
			FileInputStream in = new FileInputStream(file);
			byte[] bytes = new byte[in.available()];
			in.read(bytes);
			in.close();
			String s = new String(bytes, "UTF-8");
			String[] lines = s.split("\n");
			if (lines.length < 2 || !lines[0].startsWith("name:") || !lines[1].startsWith("pw:")) {
				System.err.println("Invalid user file: " + id);
				return null;
			}
			String name = lines[0].substring("name:".length());
			String pw = lines[1].substring("pw:".length());
			if (pw.startsWith("\"") && pw.endsWith("\"")) {
				pw = getHashValue(pw.substring(1, pw.length()-1));
			}
			Map<String, String> userdata = new HashMap<String, String>();
			for (int i = 2 ; i < lines.length ; i++) {
				String l = lines[i];
				int p = l.indexOf(":");
				if (p > -1) {
					String n = l.substring(0, p);
					String v = l.substring(p+1);
					userdata.put(n, v);
				}
			}
			return new User(id, name, pw, userdata, file);
		} catch (NumberFormatException ex) {
			System.err.println("ignoring user file: " + file.getName());
		} catch (IOException ex) {
			System.err.println("cannot read user file: " + file.getName());
		}
		return null;
	}
	
	/**
	 * Saves the data of this user in a file on the server.
	 */
	private void save() {
		try {
			FileOutputStream out = new FileOutputStream(file);
			out.write(serialize().getBytes("UTF-8"));
			out.close();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}
	
	/**
	 * Creates a new user.
	 * 
	 * @param id The user id.
	 * @param name The name of the user.
	 * @param pw The password in plain text.
	 * @param userdata The user data map.
	 * @param file The file on the server that stores the user data.
	 * @return The new user object.
	 */
	static User createUser(long id, String name, String pw, Map<String, String> userdata, File file) {
		return new User(id, name, getHashValue(pw), userdata, file);
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
		pwHash = getHashValue(newPw);
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
		return getHashValue(pw).equals(pwHash);
	}
	
	/**
	 * Returns a serialized form of the user data, which is used to store the user data in a file
	 * on the server side.
	 * 
	 * @return The serialized user data.
	 */
	private String serialize() {
		String ud = "";
		List<String> keys = new ArrayList<String>(userdata.keySet());
		Collections.sort(keys);
		for (String k : keys) {
			String v = userdata.get(k);
			if (v != null && v.length() > 0) {
				ud += k + ":" + v + "\n";
			}
		}
		return "name:" + name + "\n" + "pw:" + pwHash + "\n\n" + ud;
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
