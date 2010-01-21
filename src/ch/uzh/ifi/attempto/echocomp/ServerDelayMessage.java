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

package ch.uzh.ifi.attempto.echocomp;

import nextapp.echo2.webrender.output.HtmlDocument;
import nextapp.echo2.webrender.output.XmlDocument;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This class can be used to create user-defined messages in the case of server delay.
 * 
 * @author Tobias Kuhn
 */
public class ServerDelayMessage extends nextapp.echo2.webrender.ServerDelayMessage {

	private static final long serialVersionUID = 2045507176304630957L;
	
	private Element element;
	
	/**
	 * Creates a new server delay message.
	 * 
	 * @param message The message text.
	 */
	public ServerDelayMessage(String message) {
		this(message, null);
	}
	
	/**
	 * Creates a new server delay message.
	 * 
	 * @param message The message text.
	 * @param imageLocation A link to the image which is displayed above the text.
	 */
	public ServerDelayMessage(String message, String imageLocation) {
		XmlDocument xmlDocument = new XmlDocument("div", null, null, HtmlDocument.XHTML_1_0_NAMESPACE_URI);
		Document document = xmlDocument.getDocument();
		Element divElement = document.getDocumentElement();
		divElement.setAttribute("id", ELEMENT_ID_MESSAGE);
		divElement.setAttribute("style",
				"position:absolute;" +
				"top:0px;" +
				"left:0px;" +
				"width:100%;" +
				"height:100%;" +
				"cursor:wait;" +
				"margin:0px;" +
				"padding:0px;" +
				"visibility:hidden;" +
				"z-index:10000;"
			);
		
		Element tableElement = document.createElement("table");
		tableElement.setAttribute("style",
				"width:100%;" +
				"height:100%;" +
				"border:0px;" +
				"padding:0px;"
		);
		divElement.appendChild(tableElement);
		
		Element tbodyElement = document.createElement("tbody");
		tableElement.appendChild(tbodyElement);
		
		Element trElement = document.createElement("tr");
		tbodyElement.appendChild(trElement);
		
		Element tdElement = document.createElement("td");
		trElement.appendChild(tdElement);
		
		Element messageElement = document.createElement("div");
		messageElement.setAttribute("id", ELEMENT_ID_LONG_MESSAGE);
		messageElement.setAttribute("style",
				"margin-top:40px;" +
				"margin-left:auto;" +
				"margin-right:auto;" +
				"background-color:#e6e6e6;" +
				"color:#000000;" +
				"padding:40px;" +
				"width:200px;" +
				"border:solid 2px #000000;" +
				"font-family:verdana,arial,helvetica,sans-serif;" +
				"font-size:10pt;" +
				"text-align:left;"
			);
		
		if (imageLocation != null) {
			Element img = document.createElement("img");
			img.setAttribute("src", imageLocation);
			messageElement.appendChild(img);
		}
		
		messageElement.appendChild(document.createElement("br"));
		messageElement.appendChild(document.createElement("br"));
		
		messageElement.appendChild(document.createTextNode(message));
		
		tdElement.appendChild(messageElement);
		element = divElement;
	}
	
	public Element getMessage() {
		return element;
	}

}
