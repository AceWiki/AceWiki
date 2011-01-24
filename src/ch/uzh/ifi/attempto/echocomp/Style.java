// This file is part of AceWiki.
// Copyright 2008-2011, Tobias Kuhn.
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

import nextapp.echo.app.Color;
import nextapp.echo.app.Font;
import nextapp.echo.app.StyleSheet;
import nextapp.echo.app.serial.SerialException;
import nextapp.echo.app.serial.StyleSheetLoader;

/**
 * This class defines some style attributes that are used by the components of this package.
 * 
 * @author Tobias Kuhn
 */
public class Style {
    
	private Style() {}  // no instances allowed
    
    static {
        try {
            styleSheet = StyleSheetLoader.load(
            		"ch/uzh/ifi/attempto/echocomp/style/Default.stylesheet.xml",
            		Thread.currentThread().getContextClassLoader()
            	);
        } catch (SerialException ex) {
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
     * The color for shaded background areas.
     */
    public static Color shadedBackground = new Color(240, 240, 240);
    
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
    public static Color lightDisabled = new Color(220, 220, 220);
    
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
    
    /**
     * This method is used to modify a given color by keeping its brightness and saturation. A
	 * shift value of 120, for example, means a shift by 120 "degrees" towards violet. A shift of
	 * 360 is a full rotation and result in the original color.
     * 
     * @param c The original color.
     * @param colorShift The color shift value.
     * @return The modified color.
     */
	public static Color shiftColor(Color c, int colorShift) {
		if (colorShift >= 240) {
			c = new Color(c.getGreen(), c.getBlue(), c.getRed());
		} else if (colorShift >= 120) {
			c = new Color(c.getBlue(), c.getRed(), c.getGreen());
		}
		double s = (colorShift % 120) / 120.0;
		Color color = new Color(
				(int) (s * c.getBlue() + (1-s) * c.getRed()),
				(int) (s * c.getRed() + (1-s) * c.getGreen()),
				(int) (s * c.getGreen() + (1-s) * c.getBlue())
			);
		return color;
	}

}
