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

package ch.uzh.ifi.attempto.echocomp;

import nextapp.echo2.app.Color;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.StyleSheet;
import nextapp.echo2.app.componentxml.ComponentXmlException;
import nextapp.echo2.app.componentxml.StyleSheetLoader;

/**
 * This class defines some style attributes that are used by the components of this package.
 * 
 * @author Tobias Kuhn
 */
public class Style {
    
	private Style() {}  // no instances allowed
    
    static {
        try {
            styleSheet = StyleSheetLoader.load("ch/uzh/ifi/attempto/echocomp/style/Default.stylesheet", 
                    Thread.currentThread().getContextClassLoader());
        } catch (ComponentXmlException ex) {
            throw new RuntimeException(ex);
        }
    }
	
	/**
	 * The style sheet containing the shadows for internal windows.
	 */
    public static StyleSheet styleSheet;
    
    /**
     * The light background color.
     */
    public static Color lightBackground = new Color(190, 190, 255);

    /**
     * The medium background color.
     */
    public static Color mediumBackground = new Color(160, 160, 255);

    /**
     * The dark background color.
     */
    public static Color darkBackground = new Color(60, 60, 220);
    
    /**
     * The light foreground color.
     */
    public static Color lightForeground = new Color(255, 255, 255);
    
    /**
     * The medium foreground color.
     */
    public static Color mediumForeground = new Color(60, 60, 220);
    
    /**
     * The dark foreground color.
     */
    public static Color darkForeground = new Color(0, 0, 0);
    
    /**
     * The light color for disabled components
     */
    public static Color lightDisabled = new Color(200, 200, 200);
    
    /**
     * The dark color for disabled components
     */
    public static Color darkDisabled = new Color(100, 100, 100);
    
    /**
     * The color for the title background of internal windows.
     */
    public static Color windowTitleBackground = new Color(110, 110, 210);
    
    /**
     * The font typeface.
     */
    public static Font.Typeface fontTypeface = Font.VERDANA;

}
