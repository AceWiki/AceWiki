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

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableSet;

import ch.uzh.ifi.attempto.acewiki.core.AbstractModuleElement;
import ch.uzh.ifi.attempto.acewiki.core.ModuleElement;
import ch.uzh.ifi.attempto.acewiki.core.Ontology;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.gfservice.GfModule;
import ch.uzh.ifi.attempto.gfservice.GfParseResult;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;

/**
 * Page that represents a GF grammar module
 *
 * @author Kaarel Kaljurand
 */
public class TypeGfModule extends AbstractModuleElement implements ModuleElement {

	private final Logger mLogger = LoggerFactory.getLogger(TypeGfModule.class);
	private static final Pattern PATTERN_IDENT = Pattern.compile("[A-Za-z_][A-Za-z0-9_']*");

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

	public Set<ModuleElement> getReferencedModules() {
		// TODO: calculate it after every change, and return the calculated value here
		return ImmutableSet.copyOf(getReferencedModules(getModuleContent().getText(), getOntology()));
	}

	@Override
	public boolean references(ModuleElement module) {
		return getReferencedModules().contains(module);
	}

	public static Iterable<String> tokenizeText(String text) {
		return Splitter.on("~b").split(text.replaceAll("([a-zA-Z0-9_-]+)", "~b$1~b"));
	}

	public static Iterable<ModuleElement> getReferencedModules(String moduleText, final Ontology ont) {
		final Matcher matcher = PATTERN_IDENT.matcher(moduleText);
		return new Iterable<ModuleElement>() {

			@Override
			public Iterator<ModuleElement> iterator() {
				return new Iterator<ModuleElement>() {

					ModuleElement mNextModule = null;

					@Override
					public ModuleElement next() throws NoSuchElementException {
						if (mNextModule != null) {
							ModuleElement r = mNextModule;
							mNextModule = null;
							return r;
						}
						while (matcher.find()) {
							OntologyElement oe = ont.getElement(matcher.group());
							if (oe != null && oe instanceof ModuleElement) {
								return (ModuleElement) oe;
							}
						}
						throw new NoSuchElementException();
					}

					@Override
					public boolean hasNext() {
						if (mNextModule != null) {
							return true;
						}
						try {
							mNextModule = next();
							return true;
						} catch (NoSuchElementException e) {
							return false;
						}
					}

					@Override
					public void remove() {
						throw new UnsupportedOperationException();
					}

				};
			}

		};
	}

}