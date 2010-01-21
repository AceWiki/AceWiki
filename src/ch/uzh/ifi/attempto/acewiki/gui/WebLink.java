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

import ch.uzh.ifi.attempto.echocomp.Style;
import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.app.Button;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.ImageReference;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.webcontainer.command.BrowserRedirectCommand;

/**
 * This class represents a web link that points to a foreign website.
 * 
 * @author Tobias Kuhn
 */
public class WebLink extends Button implements ActionListener {
	
	private static final long serialVersionUID = -7315714524602560410L;
	
	private String url;
	
	/**
	 * Creates a new web link.
	 * 
	 * @param url The URL of the website.
	 * @param text The link text.
	 * @param verbose If true then the URL is included in the link text.
	 */
	public WebLink(String url, String text, boolean verbose) {
		if (verbose) {
			setText(text + " (" + url + ")");
		} else {
			setText(text);
		}
		this.url = url;
		initButton();
	}
	
	/**
	 * Creates a new web link.
	 * 
	 * @param url The URL of the website.
	 * @param text The link text.
	 */
	public WebLink(String url, String text) {
		this(url, text, false);
	}
	
	/**
	 * Creates a new web link with the URL as the link text.
	 * 
	 * @param url The URL of the website.
	 */
	public WebLink(String url) {
		this(url, url, false);
	}
	
	/**
	 * Creates a new web link consisting of an image.
	 * 
	 * @param url The URL of the website.
	 * @param image The image that acts as a link.
	 */
	public WebLink(String url, ImageReference image) {
		super(image);
		this.url = url;
		initButton();
	}
	
	private void initButton() {
		setFont(new Font(Style.fontTypeface, Font.ITALIC, new Extent(13)));
		setForeground(Style.mediumForeground);
		setLineWrap(false);
		setRolloverEnabled(true);
		setRolloverForeground(Color.BLUE);
		addActionListener(this);
	}

	public void actionPerformed(ActionEvent e) {
		ApplicationInstance.getActive().enqueueCommand(new BrowserRedirectCommand(url));
	}

}
