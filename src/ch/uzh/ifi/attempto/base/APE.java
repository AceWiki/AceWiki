// This file is part of AceWiki.
// Copyright 2008-2011, AceWiki developers.
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

package ch.uzh.ifi.attempto.base;

import java.util.*;

import ch.uzh.ifi.attempto.ape.ACEParser;
import ch.uzh.ifi.attempto.ape.APESocket;
import ch.uzh.ifi.attempto.ape.APELocal;
import ch.uzh.ifi.attempto.ape.APEWebservice;

/**
 * This class is used to get an instance of ACEParser by parameters.
 *
 * Following parameters are recognized:
 * - apetype
 *   Specify the type of APE, should be one of "local", "socket" and "webservice".
 * - apecommand
 *   Location of ape.exe, used by APELocal when apetype is "local".
 * - apehost
 *   Host name of APE socket server, used by APESocket when apetype is "socket".
 * - apeport
 *   Port number of APE socket server, used by APESocket whe apetype is "socket".
 * - apewebservice
 *   APE webservice URL, use by APEWebservice when apetype is "webservice".
 *
 * @author Yu Changyuan
 */

public class APE {

    private static Map<String, String> parameters = new HashMap<String, String>();

    /**
     * Get a ACEParser instance by parameters.
     *
     * @param parameters The parameters set in servlet web.xml.
     * @return The ACEParser instance.
     */
    public static ACEParser getParser(Map<String, String> parameters) {
        String apeType = parameters.get("context:apetype");
        if (apeType == null) apeType = "local";

        if (apeType.equals("socket")) {
            String host = parameters.get("context:apehost");
            if (host == null) host = "127.0.0.1";
            int port;
            try {
                port = Integer.parseInt(parameters.get("context:apeport"));
            }
            catch (NumberFormatException e) {
                port = 0xace;
            }

            return getAPESocket(host, port);
        }

        if (apeType.equals("webservice")) {
            String url = parameters.get("context:apewebservice");
            if (url == null) url = "http://127.0.0.1:8000/";

            return getAPEWebservice(url);
        }

        String apeCommand = parameters.get("context:apecommand");
        if (apeCommand == null) apeCommand = "ape.exe";

        return getAPELocal(apeCommand);
    }

    /**
     * Get a ACEParser instance with default parameters.
     *
     * @return The ACEParser instance.
     */
    public static ACEParser getParser() {
        return getParser(parameters);
    }

    /**
     * Set default parameters.
     *
     * @param parameters The parameters to set.
     */
    public static void setParameters(Map<String, String> parameters) {
        APE.parameters = parameters;
    }

    /**
     * Get a APELocal instance with specified ape.exe location
     *
     * @param apeCommand The location of ape.exe.
     * @return The APELocal instance.
     */
    public static ACEParser getAPELocal(String apeCommand) {
        if (!APELocal.isInitialized()) {
            APELocal.init(apeCommand);
        }
        return APELocal.getInstance();
    }

    /**
     * Get a APESocket instance with specified host and port
     *
     * @param host The host of APE socket server.
     * @param port The port of APE socket server.
     * @return The APESocket instance.
     */
    public static ACEParser getAPESocket(String host, int port) {
        return new APESocket(host, port);
    }

    /**
     * Get a APEWebservice instance with specified host and port
     *
     * @param url The URL of APE webservice.
     * @return The APEWebservice instance.
     */
    public static ACEParser getAPEWebservice(String url) {
        return new APEWebservice(url);
    }

}
