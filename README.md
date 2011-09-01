AceWiki
=======

Copyright 2008-2011, Tobias Kuhn.

AceWiki is a semantic wiki making use of the controlled natural language ACE.

Website: http://attempto.ifi.uzh.ch/acewiki/


Repository
----------

AceWiki is hosted on GitHub (repository and bug tracking): https://github.com/AceWiki/AceWiki

The downloads can be found here: https://github.com/AceWiki/AceWiki/downloads

Previously, Launchpad was used instead of GitHub. Old packages and old bug reports can still be
found there: https://launchpad.net/acewiki


License
-------

AceWiki is free software licensed under the GNU Lesser General Public Licence (see
licenses/gpl-3.txt and http://www.gnu.org/licenses/lgpl.html).

See lib/README.txt for information about the used third-party libraries.


Content
-------

This package contains the semantic wiki AceWiki and the ACE Editor, which is a general-purpose
editor with support for predictive text editing. Both tools rely on the language ACE, which is a
controlled natural language, concretely a subset of natural English. See
http://attempto.ifi.uzh.ch/site/ for more information about ACE.

The code for AceWiki and the ACE Editor is divided into four jar-files:

- attempto-echo.jar contains basic GUI components and a predictive editor.
- attempto-chartparser.jar contains a chart parser (concretely an Earley parser).
- attempto-acewiki.jar contains the AceWiki application.
- attempto-aceeditor.jar contains the ACE Editor application.

See the Javadoc for the detailed documentation of the packages and classes.

The following picture shows the dependencies of the different Java packages (packages import only
from lower packages):

      _______________________
     |        acewiki        |
     |   _____    ________   |
     |  |_gui_|  |_aceowl_|  |
     |   _________________   |
     |  |______core_______|  |   ___________
     |_______________________|  |_aceeditor_|
      ______________________________________
     |______________preditor________________|
      _________________    _________________
     |___chartparser___|  |____echocomp_____|


Compilation
-----------

You do not have to perform the steps described here if you are using a pre-compiled package (unless
you change the source code).

Ant is required to manually run the compilation. Make sure that a recent version of Apache Ant is
installed. The following commands are available:

- `ant compile` compiles the Java source code.
- `ant createjars` creates the jar-files.
- `ant createjavadoc` creates the Javadoc documentation pages.
- `ant clean` deletes all automatically generated files like the compiled Java classes, the
  jar-files, and the Javadoc files.
- `ant buildeverything` builds everything from scratch.


APE
---

AceWiki and the ACE Editor make use of APE, a parser that translates ACE into logic. In order to
run APE, a recent version of SWI Prolog has to be installed and the file "ape.exe" has to be
available. The file "ape.exe" can be complied from the APE package that can be obtained on the
Attempto download page: http://attempto.ifi.uzh.ch/site/downloads/

Furthermore, you have to use the following Java VM argument that points to the location where the
SWI Prolog library libjpl.jnilib (under Mac OS X), jpl.dll (under Windows), or libjpl.so (under
Linux) is located. For example:

    -Djava.library.path="/usr/lib/pl-5.8.3/lib/i386-linux"

Under Linux, the environment variable LD_PRELOAD has to be set additionally to refer to the SWI
Prolog library file:

    export LD_PRELOAD=/usr/lib/pl-5.8.3/lib/i386-linux/libjpl.so


Java
----

Under some circumstances, the environment variable LD_LIBRARY_PATH has to be set. This can be
done as follows:

    export LD_LIBRARY_PATH=/usr/lib/jvm/java-6-openjdk/jre/lib/i386:$LD_LIBRARY_PATH
    export LD_LIBRARY_PATH=/usr/lib/jvm/java-6-openjdk/jre/lib/i386/server:$LD_LIBRARY_PATH

Note that the exact paths are most probably different on your system. See also the script
"run-webapps-with-jettyrunner.sh".

If AceWiki is run on a server without a graphical interface loaded, you should additionally use the
following Java option:

    -Djava.awt.headless=true

This prevents Java from trying to use graphical libraries that might not be available.


Web Applications
----------------

AceWiki and the ACE Editor are web applications that have to be run as Java servlets. In order to
build the war-file from the AceWiki sources, you have to run the following Ant command:

    ant createwebapps

Executing this command will create the web application file "acewiki.war" and generate some files
in the folder "webapps", which contains the uncompressed content of the archive. You should have a
look at the following two files and change them if required:

    war/WEB-INF/web.xml
    war/index.html

After changing the files you should again run the "createwebapps"-command to update the war-file.

In order to run the war-file, a servlet container like Jetty or Apache Tomcat is needed. You have
to make sure that the Java VM parameter "java.library.path" is set and points to the path of the
SWI Prolog JPL library, as explained above.

Probably the easiest way to run the war-file is using Jetty Runner. The jar-file containing Jetty
Runner can be downloaded from this repository:

>   http://mirrors.ibiblio.org/pub/mirrors/maven2/org/mortbay/jetty/jetty-runner/

Using Jetty Runner, the web applications can be started with

    java -Djava.library.path=LIBJPLPATH -jar jetty-runner.jar acewiki.war

where LIBJPLPATH is the path of the SWI Prolog JPL library, or you can use the Unix shell script
"run-webapps-with-jettyrunner.sh".


AceWiki Data
------------

The AceWiki data is stored on the server in a directory called "data" (unless specified otherwise
in the web.xml file). Each AceWiki instance gets its own subdirectory therein. In order to import an
AceWiki data file "*.acewikidata", this file needs to have the name of the ontology into which it
should be imported (as defined by the web.xml file) and needs to be located in the "data"
directory. Furthermore, the AceWiki data file is only loaded if no subdirectory with the respective
ontology name exists. Thus, if there already exists data, the respective directory has to be
removed or renamed before an AceWiki data file can be imported.


Help
----

If you encounter problems, you can get help from the community. Bugs and problems should be
reported to the AceWiki site on GitHub: https://github.com/AceWiki/AceWiki
