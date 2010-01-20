// This file is part of the Attempto Java Packages.
// Copyright 2008-2009, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Java Packages is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// The Attempto Java Packages is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Java Packages. If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.acewiki.gui.page;

import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Grid;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.Window;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.gui.Title;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.PasswordField;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a page where the users can login. Note that the login feature
 * is implemented only very rudimentary at the moment. The password is always "password".
 * 
 * @author Tobias Kuhn
 */
public class LoginScreen extends Column implements ActionListener {
	
	private static final long serialVersionUID = -6704597832001286479L;
	
	private Wiki wiki;
	private Window window;
	private boolean passwordRequired;
	
	private TextField usernameField = new TextField();
	private PasswordField passwordField = new PasswordField();
	
	/**
	 * Creates a new login screen.
	 * 
	 * @param wiki The wiki instance.
	 * @param window The window in which the login screen is shown.
	 * @param title The title to be displayed.
	 * @param passwordRequired true if a password is required (not fully implemented).
	 */
	public LoginScreen(Wiki wiki, Window window, String title, boolean passwordRequired) {
		this.wiki = wiki;
		this.window = window;
		this.passwordRequired = passwordRequired;
		
		wiki.log("logi", "login page");
		
		add(new VSpace(20));
		add(new Title(title + " - Login", true));
		add(new VSpace(20));
		
		Grid grid = new Grid(2);
		grid.setInsets(new Insets(10, 5, 0, 0));
		grid.add(new SolidLabel("username:", Font.ITALIC));
		usernameField.setWidth(new Extent(200));
		usernameField.addActionListener(this);
		grid.add(usernameField);
		if (passwordRequired) {
			grid.add(new SolidLabel("password:", Font.ITALIC));
			passwordField.setWidth(new Extent(200));
			passwordField.addActionListener(this);
			grid.add(passwordField);
		}
		add(grid);
		
		add(new VSpace(30));
		
		Row r3 = new Row();
		r3.setInsets(new Insets(10, 0));
		r3.setCellSpacing(new Extent(5));
		r3.add(new GeneralButton("Login", this));
		add(r3);
	}

	public void actionPerformed(ActionEvent e) {
		wiki.log("logi", "pressed: login");
		String username = usernameField.getText();
		if (passwordRequired) {
			String password = passwordField.getText();
			String correctPassword = getPassword(username);
			if (password.equals(correctPassword)) {
				wiki.log("logi", "correct password for " + username);
				wiki.setUsername(username);
				wiki.log("syst", "login");
				window.setContent(wiki.getContentPane());
			} else {
				wiki.log("logi", "incorrect password: " + username);
				window.getContent().add(new MessageWindow(
						"Error",
						"Invalid username or password!",
						"OK"
					));
			}
		} else {
			if (username.equals("")) {
				wiki.log("logi", "no username entered");
				window.getContent().add(new MessageWindow(
						"Error",
						"Please enter your name!",
						"OK"
					));
			} else {
				wiki.log("logi", "username: " + username);
				wiki.setUsername(username);
				wiki.log("syst", "login");
				window.setContent(wiki.getContentPane());
			}
		}
	}
	
	private String getPassword(String username) {
		// Put your code here that returns the password for the given username!
		return "password";
	}
	
}
