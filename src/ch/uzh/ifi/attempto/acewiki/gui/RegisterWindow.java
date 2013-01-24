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

package ch.uzh.ifi.attempto.acewiki.gui;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Column;
import nextapp.echo.app.Extent;
import nextapp.echo.app.Font;
import nextapp.echo.app.Grid;
import nextapp.echo.app.Insets;
import nextapp.echo.app.Row;
import nextapp.echo.app.WindowPane;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.layout.GridLayoutData;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.User;
import ch.uzh.ifi.attempto.acewiki.core.UserBase;
import ch.uzh.ifi.attempto.echocomp.CheckBox;
import ch.uzh.ifi.attempto.echocomp.GeneralButton;
import ch.uzh.ifi.attempto.echocomp.Label;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.PasswordField;
import ch.uzh.ifi.attempto.echocomp.SolidLabel;
import ch.uzh.ifi.attempto.echocomp.Style;
import ch.uzh.ifi.attempto.echocomp.TextField;
import ch.uzh.ifi.attempto.echocomp.VSpace;

/**
 * This class represents a window for the registration of a new user.
 * 
 * @author Tobias Kuhn
 */
public class RegisterWindow extends WindowPane implements ActionListener {
	
	private static final long serialVersionUID = -6704597832001286479L;
	
	private Wiki wiki;
	
	private TextField usernameField = new TextField(250, this, Font.ITALIC);
	private TextField emailField = new TextField(250, this, Font.ITALIC);
	private PasswordField passwordField = new PasswordField(250, this);
	private PasswordField retypePasswordField = new PasswordField(250, this);
	private CheckBox stayLoggedInCheckBox = new CheckBox();
	
	/**
	 * Creates a new registration window.
	 * 
	 * @param username The default username (from the login window).
	 * @param password The default password (from the login window).
	 * @param stayLoggedIn The default value for staying logged in (from the login window).
	 * @param wiki The wiki instance.
	 */
	public RegisterWindow(String username, String password, boolean stayLoggedIn, Wiki wiki) {
		this.wiki = wiki;
		
		usernameField.setText(username);
		passwordField.setText(password);
		stayLoggedInCheckBox.setSelected(stayLoggedIn);
		
		setTitle(wiki.getGUIText("acewiki_userwindow_registertitle"));
		setTitleFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setModal(true);
		setWidth(new Extent(500));
		setHeight(new Extent(290));
		setResizable(false);
		setMovable(true);
		setClosable(!wiki.isLoginRequiredForViewing());
		setTitleBackground(Style.windowTitleBackground);
		setStyleName("Default");
		
		wiki.log("logi", "registration window");

		Grid mainGrid = new Grid(1);
		mainGrid.setInsets(new Insets(10, 10, 0, 0));
		mainGrid.setColumnWidth(0, new Extent(480));
		mainGrid.setRowHeight(0, new Extent(200));
		
		Column messageColumn = new Column();
		GridLayoutData layout1 = new GridLayoutData();
		layout1.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		messageColumn.setLayoutData(layout1);
		Label label = new Label(wiki.getGUIText("acewiki_userwindow_registermessage"));
		label.setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		messageColumn.add(label);
		messageColumn.add(new VSpace());
		
		Grid formGrid = new Grid(2);
		formGrid.setInsets(new Insets(10, 10, 0, 0));
		formGrid.add(new SolidLabel(wiki.getGUIText("acewiki_userwindow_username"), Font.ITALIC));
		formGrid.add(usernameField);
		formGrid.add(new SolidLabel(wiki.getGUIText("acewiki_userwindow_email"), Font.ITALIC));
		formGrid.add(emailField);
		formGrid.add(new SolidLabel(wiki.getGUIText("acewiki_userwindow_password"), Font.ITALIC));
		formGrid.add(passwordField);
		formGrid.add(new SolidLabel(wiki.getGUIText("acewiki_userwindow_retypepassword"), Font.ITALIC));
		formGrid.add(retypePasswordField);
		formGrid.add(new SolidLabel(wiki.getGUIText("acewiki_userwindow_stayloggedin"), Font.ITALIC));
		formGrid.add(stayLoggedInCheckBox);
		messageColumn.add(formGrid);
		
		mainGrid.add(messageColumn);

		Row buttonBar = new Row();
		buttonBar.setCellSpacing(new Extent(10));
		buttonBar.setInsets(new Insets(0, 0, 0, 10));
		buttonBar.add(new GeneralButton("acewiki_userwindow_registerbutton", this, 100));
		buttonBar.add(new GeneralButton("general_action_cancel", this, 100));
		GridLayoutData layout2 = new GridLayoutData();
		layout2.setAlignment(new Alignment(Alignment.CENTER, Alignment.BOTTOM));
		buttonBar.setLayoutData(layout2);
		mainGrid.add(buttonBar);
		
		add(mainGrid);
		
		wiki.getApplication().setFocusedComponent(usernameField);
	}

	public void actionPerformed(ActionEvent e) {
		UserBase ub = wiki.getUserBase();
		String username = usernameField.getText();
		String password = passwordField.getText();
		String password2 = retypePasswordField.getText();
		String email = emailField.getText();
		boolean stayLoggedIn = stayLoggedInCheckBox.isSelected();
		if ("general_action_cancel".equals(e.getActionCommand())) {
			wiki.log("logi", "registration canceled");
			setVisible(false);
			wiki.removeWindow(this);
			if (wiki.isLoginRequiredForViewing()) {
				wiki.showLoginWindow();
			}
		} else {
			wiki.log("logi", "pressed: register");
			if (username.length() < 3 || username.length() > 20) {
				wiki.log("logi", "invalid username");
				wiki.showWindow(new MessageWindow(
						"acewiki_error_title",
						"acewiki_error_usernamelength",
						"general_action_ok"
					));
			} else if (!username.matches("[a-zA-Z0-9'.][a-zA-Z0-9'._\\- ]*[a-zA-Z0-9'.]")) {
				wiki.log("logi", "invalid username");
				wiki.showWindow(new MessageWindow(
						"acewiki_error_title",
						"acewiki_error_usernamechars",
						"general_action_ok"
					));
			} else if (password.length() < 5) {
				wiki.log("logi", "invalid password");
				wiki.showWindow(new MessageWindow(
						"acewiki_error_title",
						"acewiki_error_passwordlength",
						"general_action_ok"
					));
			} else if (!password.equals(password2)) {
				wiki.log("logi", "retype password does not match");
				wiki.showWindow(new MessageWindow(
						"acewiki_error_title",
						"acewiki_error_retypepassword",
						"general_action_ok"
					));
			} else if (email.indexOf("@") < 0) {
				wiki.log("logi", "no email");
				wiki.showWindow(new MessageWindow(
						"acewiki_error_title",
						"acewiki_error_email",
						"general_action_ok"
					));
			} else {
				User user = ub.register(username, email, password);
				if (user == null) {
					wiki.log("logi", "username already taken: " + username);
					wiki.showWindow(new MessageWindow(
							"acewiki_error_title",
							"acewiki_error_usernametaken",
							"general_action_ok"
						));
				} else {
					wiki.log("logi", "register successful for " + username);
					wiki.login(user, stayLoggedIn);
					wiki.removeWindow(this);
				}
			}
		}
	}
	
}
