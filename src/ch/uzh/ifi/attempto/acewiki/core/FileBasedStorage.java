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

package ch.uzh.ifi.attempto.acewiki.core;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class FileBasedStorage implements AceWikiStorage {
	
	private final HashMap<String, Ontology> ontologies = new HashMap<String, Ontology>();
	private final Map<String, UserBase> userBases = new HashMap<String, UserBase>();
	private String dir;
	private final List<Ontology> incompleteOntologies = new ArrayList<Ontology>();
	
	public FileBasedStorage(String dir) {
		this.dir = dir.replaceFirst("/*$", "");
	}
	
	/**
	 * Returns the ontology with the given name (or creates an empty ontology if the ontology
	 * cannot be found). A parameter map is used for ontology parameters. When the ontology with
	 * the respective name has already been loaded, this ontology is returned and the parameters
	 * are ignored. The following parameters are supported:
	 * "baseuri": The base URI that is used to identify the ontology elements. The complete URI of
	 *     the ontology is baseURI + name. The default is an empty string.
	 * "global_restrictions_policy": A string representing the policy how to enforce the global
	 *     restrictions on axioms in OWL 2. At the moment, the options "no_chains" and "unchecked"
	 *     are available.
	 * "reasoner": Defines the reasoner or reasoner interface to be used. Currently supported are
	 *     the HermiT reasoner ("HermiT", default), the Pellet reasoner ("Pellet"), the OWLlink
	 *     interface ("OWLlink"), or none ("none").
	 * "owl_profile": Sets an OWL profile that defines which statements are used for reasoning.
	 *     Possible values are "OWL2Full" (default), "OWL2EL", "OWL2QL", and "OWL2RL". Note that
	 *     the global restrictions of the EL profile are not checked.
	 * 
	 * @param name The name of the ontology.
	 * @param parameters The parameters.
	 * @return The loaded ontology.
	 */
	public Ontology getOntology(String name, Map<String, String> parameters) {
		if (ontologies.get(name) != null) {
			return ontologies.get(name);
		}
		return loadOntology(name, parameters);
	}
	
	private synchronized Ontology loadOntology(String name, Map<String, String> parameters) {
		if (ontologies.get(name) != null) {
			return ontologies.get(name);
		}
		Ontology ontology = new Ontology(name, parameters, this);
		incompleteOntologies.add(ontology);
		ontologies.put(name, ontology);
		ontology.log("loading ontology");
		System.err.println("Loading '" + name + "'");
		File dataDir = new File(dir + "/" + name);
		File dataFile = new File(dir + "/" + name + ".acewikidata");
		if (dataDir.exists()) {
			System.err.print("Entities:   ");
			ConsoleProgressBar pb1 = new ConsoleProgressBar(dataDir.listFiles().length);
			for (File file : dataDir.listFiles()) {
				pb1.addOne();
				try {
					long id = new Long(file.getName());
					FileInputStream in = new FileInputStream(file);
					byte[] bytes = new byte[in.available()];
					in.read(bytes);
					in.close();
					String s = new String(bytes, "UTF-8");
					loadOntologyElement(s, id, ontology);
				} catch (NumberFormatException ex) {
					ontology.log("ignoring file: " + file.getName());
				} catch (IOException ex) {
					ontology.log("cannot read file: " + file.getName());
				}
			}
			pb1.complete();
		} else if (dataFile.exists()) {
			System.err.print("Entities:   ");
			ConsoleProgressBar pb1 = null;
			try {
				BufferedReader in = new BufferedReader(new FileReader(dataFile));
				pb1 = new ConsoleProgressBar(dataFile.length());
				String s = "";
				String line = in.readLine();
				while (line != null) {
					pb1.add(line.length() + 1);
					if (line.matches("\\s*")) {
						// empty line
						if (s.length() > 0) {
							loadOntologyElement(s, -1, ontology);
							s = "";
						}
					} else if (line.startsWith("%")) {
						// comment
					} else {
						s += line + "\n";
					}
					line = in.readLine();
				}
				in.close();
			} catch (IOException ex) {
				ontology.log("cannot read file: " + dataFile.getName());
			}
			if (pb1 != null) pb1.complete();
		} else {
			ontology.log("no data found; blank ontology is created");
		}

		ontology.log("loading statements");
		System.err.print("Statements: ");
		List<OntologyElement> elements = ontology.getOntologyElements();
		ConsoleProgressBar pb2 = new ConsoleProgressBar(elements.size());
		for (OntologyElement oe : elements) {
			pb2.addOne();
			ontology.getReasonerManager().loadElement(oe);
			for (Sentence s : oe.getArticle().getSentences()) {
				if (s.isReasonerParticipant() && s.isIntegrated()) {
					ontology.getReasonerManager().loadSentence(s);
				}
			}
			save(oe);
		}
		pb2.complete();
		
		if (ontology.get(0) == null) {
			OntologyElement mainPage = new DummyOntologyElement("mainpage", "Main Page");
			mainPage.initId(0);
			ontology.register(mainPage);
		}

		incompleteOntologies.remove(ontology);
		ontology.getReasonerManager().load();
		
		return ontology;
	}
	
	/**
	 * Loads an ontology element from its serialized form.
	 * 
	 * @param serializedElement The serialized ontology element.
	 * @param id The id of the ontology element.
	 * @param ontology The ontology at which the ontology element should be registered.
	 */
	private static void loadOntologyElement(String serializedElement, long id, Ontology ontology) {
		List<String> lines = new ArrayList<String>(Arrays.asList(serializedElement.split("\n")));
		if (lines.size() == 0 || !lines.get(0).startsWith("type:")) {
			System.err.println("Cannot read ontology element (missing 'type')");
			return;
		}
		String type = lines.remove(0).substring("type:".length());
		OntologyElement oe = ontology.getLanguageFactory().createOntologyElement(type);
		if (oe != null) {
			if (!lines.get(0).startsWith("words:")) {
				System.err.println("Cannot read ontology element (missing 'words')");
				return;
			}
			String[] words = lines.remove(0).substring("words:".length()).split(";");
			oe.setWords(words);
		}
		
		// Dummy ontology element for the main page article:
		if (type.equals("mainpage")) {
			id = 0;
			oe = new DummyOntologyElement("mainpage", "Main Page");
		}
		
		oe.initOntology(ontology);
		oe.initArticle(loadArticle(lines, oe));
		oe.initId(id);
		ontology.register(oe);
		return;
	}
	
	private static Article loadArticle(List<String> lines, OntologyElement element) {
		Article a = new Article(element);
		List<Statement> statements = new ArrayList<Statement>();
		while (!lines.isEmpty()) {
			String l = lines.remove(0);
			Statement statement = loadStatement(l, a);
			if (statement == null) {
				System.err.println("Cannot read statement: " + l);
			} else {
				statements.add(statement);
			}
		}
		a.initStatements(statements);
		return a;
	}
	
	/**
	 * Loads a statement from a serialized form.
	 * 
	 * @param serializedStatement The serialized statement as a string.
	 * @param article The article of the statement.
	 * @return The new statement object.
	 */
	private static Statement loadStatement(String serializedStatement, Article article) {
		if (serializedStatement.length() < 2) return null;
		String s = serializedStatement.substring(2);
		
		Ontology ontology = article.getOntology();
		StatementFactory statementFactory = ontology.getStatementFactory();
		if (serializedStatement.startsWith("| ")) {
			Sentence sentence = statementFactory.createSentence(s, article);
			sentence.setIntegrated(true);
			return sentence;
		} else if (serializedStatement.startsWith("# ")) {
			Sentence sentence = statementFactory.createSentence(s, article);
			sentence.setIntegrated(false);
			return sentence;
		} else if (serializedStatement.startsWith("c ")) {
			String t = s.replaceAll("~n", "\n").replaceAll("~t", "~");
			return statementFactory.createComment(t, article);
		}
		
		return null;
	}
	
	public synchronized void save(OntologyElement oe) {
		Ontology o = oe.getOntology();
		String name = o.getName();
		
		// Ontology elements of incomplete ontologies are not saved at this point:
		if (incompleteOntologies.contains(o)) return;
		
		if (!(new File(dir)).exists()) (new File(dir)).mkdir();
		if (!(new File(dir + "/" + name)).exists()) (new File(dir + "/" + name)).mkdir();
		
		if (!o.contains(oe)) {
			(new File(dir + "/" + name + "/" + oe.getId())).delete();
			return;
		}
		
		try {
			FileOutputStream out = new FileOutputStream(dir + "/" + name + "/" + oe.getId());
			out.write(serialize(oe, true).getBytes("UTF-8"));
			out.close();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}
	
	/**
	 * Serializes this ontology element as a string.
	 * 
	 * @param encodeWords defines whether words should be encoded (for the internal "database") or
	 *   not (for export in the AceWiki data format).
	 * @return The serialized ontology element.
	 */
	public static String serialize(OntologyElement element, boolean encodeWords) {
		String s = "type:" + element.getInternalType() + "\n";
		if (element.getWords().length > 0) {
			s += "words:";
			for (String word : element.getWords()) {
				if (word == null) {
					s += ";";
				} else {
					s += word + ";";
				}
			}
			s += "\n";
		}
		s += element.getArticle().serialize(encodeWords);
		return s;
	}
	
	public UserBase getUserBase(Ontology ontology) {
		UserBase userBase = userBases.get(ontology.getName());
		if (userBase == null) {
			userBase = new UserBase(ontology, this);
			File userDir = new File(dir + "/" + ontology.getName() + ".users");
			if (userDir.exists()) {
				for (File file : userDir.listFiles()) {
					User user = loadUser(userBase, file);
					userBase.addUser(user);
				}
			} else {
				userDir.mkdir();
			}
		}
		return userBase;
	}
	
	private static User loadUser(UserBase userBase, File file) {
		try {
			long id = new Long(file.getName());
			FileInputStream in = new FileInputStream(file);
			byte[] bytes = new byte[in.available()];
			in.read(bytes);
			in.close();
			String s = new String(bytes, "UTF-8");
			String[] lines = s.split("\n");
			if (lines.length < 2 || !lines[0].startsWith("name:") || !lines[1].startsWith("pw:")) {
				System.err.println("Invalid user file: " + id);
				return null;
			}
			String name = lines[0].substring("name:".length());
			String pw = lines[1].substring("pw:".length());
			if (pw.startsWith("\"") && pw.endsWith("\"")) {
				pw = User.getPasswordHash(pw.substring(1, pw.length()-1));
			}
			Map<String, String> userdata = new HashMap<String, String>();
			for (int i = 2 ; i < lines.length ; i++) {
				String l = lines[i];
				int p = l.indexOf(":");
				if (p > -1) {
					String n = l.substring(0, p);
					String v = l.substring(p+1);
					userdata.put(n, v);
				}
			}
			return new User(id, name, pw, userdata, userBase);
		} catch (NumberFormatException ex) {
			System.err.println("ignoring user file: " + file.getName());
		} catch (IOException ex) {
			System.err.println("cannot read user file: " + file.getName());
		}
		return null;
	}
	
	public void save(User user) {
		try {
			String n = user.getUserBase().getOntology().getName();
			FileOutputStream out = new FileOutputStream(
					new File(dir + "/" + n + ".users" + "/" + user.getId())
				);
			out.write(serialize(user).getBytes("UTF-8"));
			out.close();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}
	
	private static String serialize(User user) {
		String ud = "";
		List<String> keys = user.getUserDataKeys();
		Collections.sort(keys);
		for (String k : keys) {
			String v = user.getUserData(k);
			if (v != null && v.length() > 0) {
				ud += k + ":" + v + "\n";
			}
		}
		return "name:" + user.getName() + "\n" + "pw:" + user.getHashedPassword() + "\n\n" + ud;
	}

}
