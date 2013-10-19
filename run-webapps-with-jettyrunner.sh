#==============================================================================
# This Unix shell script starts the AceWiki web applications with Jetty Runner.
# You can then load the application with a browser from http://localhost:9077
# (or some other port, depending on which port you specify here).
# It assumes:
# - The WAR file for AceWiki has been built from the AceWiki package, or
#   downloaded and renamed to "acewiki.war".
# - A recent version of SWI Prolog is installed.
# - APE has been compiled (giving "ape.exe").
# - Jetty Runner has been downloaded and is renamed to "jetty-runner.jar".
#   Tested with
#   curl http://repo2.maven.org/maven2/org/mortbay/jetty/jetty-runner/8.1.5.v20120716/jetty-runner-8.1.5.v20120716.jar > jetty-runner.jar
# - The files "acewiki.war", "ape.exe" and "jetty-runner.jar" are in the
#   same directory.
# - The process has write permissions to the subdirectories "data" and "logs",
#   or has the permission to create them.
# 
# (written by Tobias Kuhn with content from Jean-Marc Vanel)
# (some updates by Kaarel Kaljurand)
#==============================================================================


## On some systems, the SWI Prolog command is "pl" instead of "swipl":
eval `swipl -dump-runtime-variables`
#eval `pl -dump-runtime-variables`

## Uncomment the line below if $JAVA_HOME is not already set and make sure that
## the path matches your Java installation:
# export JAVA_HOME=/usr/lib/jvm/java-6-openjdk

## Under Linux, the environment variable LD_PRELOAD has to refer to the SWI
## Prolog library. Under some circumstances, also LD_LIBRARY_PATH has to be set.
## You might have to change "i386" to map the architecture of your system.
## (Tested with Debian 6.0.1 and Ubuntu 11.04)

## Commented the next 4 exports out because they do not apply in
## the general case, e.g. on some platforms you still get e.g.:
## java: error while loading shared libraries:
##     libjava.so: cannot open shared object file: No such file or directory
## One should specify the required paths in his/her personal .bashrc, e.g.
## export LD_LIBRARY_PATH="/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/amd64/:
## /usr/lib/jvm/java-7-openjdk-amd64/jre/lib/amd64/server/"

export LD_PRELOAD=$PLBASE/lib/$PLARCH/libjpl.so:$PLBASE/lib/$PLARCH/libswipl.so:$LD_PRELOAD
#export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/i386:$LD_LIBRARY_PATH
#export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/i386/server:$LD_LIBRARY_PATH
#export LD_LIBRARY_PATH=$PLBASE/lib/$PLARCH:$LD_LIBRARY_PATH

## The following command starts the AceWiki web application. It might be
## necessary to change port number, heap size, or stack size.
## Note that we specify the path to jpl.jar here because this jar
## is not included in the war-file.
java -Xmx400m -Xss4m \
     -Djava.library.path=$PLBASE/lib/$PLARCH \
     -Djava.awt.headless=true \
     -jar jetty-runner.jar --port 9077 --jar $PLBASE/lib/jpl.jar acewiki.war
