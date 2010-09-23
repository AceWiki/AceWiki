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

package ch.uzh.ifi.attempto.acewiki.gui;

import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Grid;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.app.layout.GridLayoutData;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.user.User;
import ch.uzh.ifi.attempto.acewiki.core.user.UserBase;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.PasswordField;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.echocomp.VSpace;
import ch.uzh.ifi.attempto.echocomp.WindowPane;

/**
 * This class represents a page where the users can login. Note that the login feature
 * is implemented only very rudimentary at the moment. The password is always "password".
 * 
 * @author Tobias Kuhn
 */
public class LoginWindow extends WindowPane implements ActionListener {
	
	private static final long serialVersionUID = -6704597832001286479L;
	
	private Wiki wiki;
	
	private TextField usernameField = new TextField();
	private PasswordField passwordField = new PasswordField();
	
	/**
	 * Creates a new login screen.
	 * 
	 * @param wiki The wiki instance.
	 */
	public LoginWindow(Wiki wiki) {
		this.wiki = wiki;
		
		setTitle("Login");
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setModal(true);
		setWidth(new Extent(470));
		setHeight(new Extent(220));
		setResizable(false);
		setMovable(true);
		setClosable(!wiki.isLoginRequiredForViewing());
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");
		
		wiki.log("logi", "login window");

		Grid mainGrid = new Grid(1);
		mainGrid.setInsets(new Insets(10, 10, 10, 0));
		mainGrid.setColumnWidth(0, new Extent(450));
		mainGrid.setRowHeight(0, new Extent(130));
		
		Column messageColumn = new Column();
		GridLayoutData layout1 = new GridLayoutData();
		layout1.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		messageColumn.setLayoutData(layout1);
		Label label = new Label("Enter your username and password:");
		label.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		messageColumn.add(label);
		messageColumn.add(new VSpace());
		
		Grid formGrid = new Grid(2);
		formGrid.setInsets(new Insets(10, 10, 10, 0));
		formGrid.add(new SolidLabel("username:", Font.ITALIC));
		usernameField.setWidth(new Extent(300));
		usernameField.addActionListener(this);
		formGrid.add(usernameField);
		formGrid.add(new SolidLabel("password:", Font.ITALIC));
		passwordField.setWidth(new Extent(300));
		passwordField.addActionListener(this);
		formGrid.add(passwordField);
		messageColumn.add(formGrid);
		
		mainGrid.add(messageColumn);

		Row buttonBar = new Row();
		buttonBar.setCellSpacing(new Extent(10));
		buttonBar.setInsets(new Insets(0, 0, 0, 10));
		buttonBar.add(new GeneralButton("Login", 80, this));
		if (wiki.isUserRegistrationOpen()) {
			buttonBar.add(new GeneralButton("Register", 80, this));
		}
		if (!wiki.isLoginRequiredForViewing()) {
			buttonBar.add(new GeneralButton("Cancel", 80, this));
		}
		GridLayoutData layout2 = new GridLayoutData();
		layout2.setAlignment(new Alignment(Alignment.CENTER, Alignment.BOTTOM));
		buttonBar.setLayoutData(layout2);
		mainGrid.add(buttonBar);
		
		add(mainGrid);
	}

	public void actionPerformed(ActionEvent e) {
		UserBase ub = wiki.getUserBase();
		String username = usernameField.getText();
		String password = passwordField.getText();
		if ("Cancel".equals(e.getActionCommand())) {
			wiki.log("logi", "login canceled");
			setVisible(false);
			wiki.removeWindow(this);
		} else if ("Register".equals(e.getActionCommand())) {
			wiki.log("logi", "pressed: signup");
			if (username.length() < 3 || username.length() > 20) {
				wiki.log("logi", "invalid username");
				wiki.showWindow(new MessageWindow(
						"Error",
						"Username needs between 3 and 20 characters.",
						"OK"
					));
			} else if (!username.matches("[a-zA-Z0-9'.][a-zA-Z0-9'._\\- ]*[a-zA-Z0-9'.]")) {
				wiki.log("logi", "invalid username");
				wiki.showWindow(new MessageWindow(
						"Error",
						"Username contains illegal characters.",
						"OK"
					));
			} else if (password.length() < 5) {
				wiki.log("logi", "invalid password");
				wiki.showWindow(new MessageWindow(
						"Error",
						"Password needs at least 5 characters.",
						"OK"
					));
			} else {
				User user = ub.register(username, password);
				if (user == null) {
					wiki.log("logi", "username already taken: " + username);
					wiki.showWindow(new MessageWindow(
							"Error",
							"Username is already taken.",
							"OK"
						));
				} else {
					wiki.log("logi", "signup successful for " + username);
					wiki.log("syst", "login");
					wiki.setUser(user);
				}
			}
		} else {
			wiki.log("logi", "pressed: login");
			User user = ub.login(username, password);
			if (user != null) {
				wiki.log("logi", "correct password for " + username);
				wiki.log("syst", "login");
				wiki.setUser(user);
			} else {
				wiki.log("logi", "incorrect username or password for " + username);
				wiki.showWindow(new MessageWindow(
						"Error",
						"Invalid username or password!",
						"OK"
					));
			}
		}
	}
	
}
