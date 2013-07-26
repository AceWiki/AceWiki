// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
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

package ch.uzh.ifi.attempto.acewiki.gf;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.uzh.ifi.attempto.acewiki.core.AbstractModuleElement;
import ch.uzh.ifi.attempto.acewiki.core.ModuleElement;
import ch.uzh.ifi.attempto.gfservice.GfModule;
import ch.uzh.ifi.attempto.gfservice.GfParseResult;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;
import ch.uzh.ifi.attempto.gfservice.GfStorageResult;

/**
 * Page that represents a GF grammar module
 *
 * @author Kaarel Kaljurand
 */
public class TypeGfModule extends AbstractModuleElement implements ModuleElement {

	private final Logger mLogger = LoggerFactory.getLogger(TypeGfModule.class);

	public static final String TYPE = "GF Module";
	public static final String INTERNAL_TYPE = "gfmodule";

	private GfEngine mEngine;

	public TypeGfModule(GfEngine engine) {
		mEngine = engine;
	}

	public String getIRISuffix() {
		return getWord(0);
	}

	public String getType() {
		return TYPE;
	}

	public String getInternalType() {
		return INTERNAL_TYPE;
	}

	public static boolean hasType(String type) {
		return INTERNAL_TYPE.equals(type);
	}

	/**
	 * @deprecated
	 */
	public void integrate() {
		if (!hasContent()) return;
		if (!mEngine.getGfGrammar().isGrammarEditable()) {
			throw new RuntimeException("Grammar is not editable");
		}
		try {
			GfStorageResult result = mEngine.getGfGrammar().integrateGfModule(getGfModule());

			if (result.isSuccess()) {
				GfWikiUtils.clearAllLinearizations(mEngine.getOntology());
			} else {
				throw new RuntimeException(result.getResultCode() + ": " +
						result.getMessage() + " (" + result.getCommand() + ")");
			}
		} catch (GfServiceException ex) {
			mLogger.info("make: GfServiceException: '{}'", ex.getMessage());
			throw new RuntimeException(ex);
		}
	}

	public void parse() throws InvalidSyntaxException {
		if (!hasContent()) return;
		try {
			GfParseResult result = mEngine.getGfGrammar().parseGfModule(getGfModule());

			if (!result.isSuccess()) {
				mLogger.info("parse: GfParseResult: '{}'", result);
				String[] pos = result.getLocation().split(":");
				throw new InvalidSyntaxException(
						result.getResultCode(),
						Integer.parseInt(pos[0]),
						Integer.parseInt(pos[1]));
			}
		} catch (GfServiceException ex) {
			mLogger.info("parse: GfServiceException: '{}'", ex.getMessage());
			throw new RuntimeException(ex);
		}
	}

	private GfModule getGfModule() {
		return new GfModule(getWord(), getModuleContent().getText());
	}

	private boolean hasContent() {
		return getModuleContent() != null;
	}

	public String getDefaultContent() {
		return "resource " + getWord() + " = {\n\n}";
	}

}