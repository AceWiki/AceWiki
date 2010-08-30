AceWiki
=======

Copyright 2008-2010, Tobias Kuhn.

Website: http://attempto.ifi.uzh.ch/acewiki/
Repository and Bug tracking: https://launchpad.net/acewiki

AceWiki is a semantic wiki making use of the controlled natural language ACE. AceWiki is free
software licensed under the GNU Lesser General Public Licence (see licenses/gpl-3.txt and
http://www.gnu.org/licenses/lgpl.html).

See lib/README.txt for information about the used third-party libraries.


1. Content
----------

This package contains the semantic wiki AceWiki and the ACE Editor, which is a general-purpose
editor with support for predictive text editing. Both tools rely on the language ACE, which is a
controlled natural language, concretely a subset of natural English. See
http://attempto.ifi.uzh.ch/site/ for more information about ACE.

The code for AceWiki and the ACE Editor is divided into four jar-files:

- attempto-echo.jar contains basic GUI components and a predictive editor.
- attempto-chartparser.jar contains a chart parser (concretely an Earley parser).
- attempto-acewiki-xxx.jar contains the AceWiki application.
- attempto-aceeditor-xxx.jar contains the ACE Editor application.

See docs/index.html for the detailed documentation of the packages and classes.


2. Compilation
--------------

This package is pre-compiled. Thus, compilation is needed only if the source code is changed. Only
the web application archive is not pre-built (see section 4). The compilation can be run manually
using Ant. Make sure that a recent version of Apache Ant is installed. The following commands are
available:

"ant compile" compiles the Java source code.

"ant createjars" creates the jar-files.

"ant createjavadoc" creates the Javadoc documentation pages.

"ant clean" deletes all automatically generated files like the compiled Java classes, the
jar-files, and the Javadoc files.

"ant buildeverything" builds everything from scratch.


3. APE
------

AceWiki and the ACE Editor make use of APE, a parser that translates ACE into logic. In order to
run APE, a recent version of SWI Prolog has to be installed and the file "ape.exe" has to be
available. The file "ape.exe" can be complied from the APE package that can be obtained on the
Attempto download page: http://attempto.ifi.uzh.ch/site/downloads/

Furthermore, you have to use the following Java VM argument that points to the location where the
SWI Prolog library libjpl.jnilib (under Mac OS X), jpl.dll (under Windows), or libjpl.so (under
Linux) is located. For example:

  -Djava.library.path="/usr/lib/pl-5.8.3/lib/i386-linux"

Under Linux, the environment variable LD_PRELOAD has to be set additionally to refer to the SWI
Prolog library file. Under some circumstances, also LD_LIBRARY_PATH has to be set. This can be done,
for example, as follows:

export LD_PRELOAD=/usr/lib/pl-5.8.3/lib/i386-linux/libjpl.so
export LD_LIBRARY_PATH=/usr/lib/jvm/java-6-openjdk/jre/lib/i386:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/usr/lib/jvm/java-6-openjdk/jre/lib/i386/server:$LD_LIBRARY_PATH

Note that the exact paths are most probably different on your system. See also the script
"run-webapps-with-jettyrunner.sh".

If AceWiki is run on a server without a graphical interface loaded, you should additionally use the
following Java option:

   -Djava.awt.headless=true

This prevents Java from trying to use graphical libraries that might not be available.


4. Web Applications
-------------------

AceWiki and the ACE Editor are web applications that have to be run as Java servlets. In order to
do so, a web application archive (WAR) file has to be built. This can be done with the following
Ant command:

  ant createwebapps

Executing this command will create the web application file "webapps.war" and generate some files
in the folder "webapps", which contains the uncompressed content of the archive. You should have a
look at the following two files and change them if required:

  webapps/war/WEB-INF/web.xml
  webapps/war/index.html

After changing the files you should again run the "createwebapps"-command to update the war-file.

In order to run the war-file, a servlet container like Jetty or Apache Tomcat is needed. You have
to make sure that the Java VM parameter "java.library.path" is set and points to the path of the
SWI Prolog JPL library, as explained above in section 3.

Probably the easiest way to run the war-file is using Jetty Runner. The jar-file containing Jetty
Runner can be downloaded from this repository:

   http://mirrors.ibiblio.org/pub/mirrors/maven2/org/mortbay/jetty/jetty-runner/

Using Jetty Runner, the web applications can be started with

  java -Djava.library.path=LIBJPLPATH -jar jetty-runner.jar webapps.war

where LIBJPLPATH is the path of the SWI Prolog JPL library, or you can use the Unix shell script
"run-webapps-with-jettyrunner.sh".


5. AceWiki Data
---------------

The AceWiki data is stored on the server in a directory called "data". Each AceWiki instance gets
its own subdirectory therein. In order to import an AceWiki data file "*.acewikidata", this file
needs to have the name of the ontology into which it should be imported (as defined by the web.xml
file) and needs to be located in the "data" directory. Furthermore, the AceWiki data file is only
loaded if no subdirectory with the respective ontology name exists. Thus, if there already exists
data, the respective directory has to be removed or renamed before an AceWiki data file can be
imported.


6. Help
-------

If you encounter problems, you can get help from the community. Bugs and questions can be submitted
to the AceWiki site on Launchpad:

  https://launchpad.net/acewiki

Alternatively, you can write to the Attempto Mailing List:

  http://attempto.ifi.uzh.ch/site/mailinglist/
