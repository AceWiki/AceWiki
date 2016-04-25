FROM ubuntu:latest
RUN apt-get -y update
RUN apt-get install -y wget git ant
RUN apt-get install -y swi-prolog swi-prolog-java
RUN apt-get install -y openjdk-7-jdk
RUN git clone --depth=1 https://github.com/Attempto/APE.git
RUN cd APE; bash make_exe.sh
RUN git clone --depth=1 https://github.com/AceWiki/AceWiki
RUN cd AceWiki; ant createwebapps
RUN mv APE/ape.exe AceWiki/
RUN wget -O AceWiki/jetty-runner.jar http://repo2.maven.org/maven2/org/mortbay/jetty/jetty-runner/8.1.9.v20130131/jetty-runner-8.1.9.v20130131.jar
ENV LD_PRELOAD /usr/lib/swi-prolog/lib/amd64/libjpl.so
ENV LD_LIBRARY_PATH /usr/lib/jvm/default-java/jre/lib/amd64:/usr/lib/jvm/default-java/jre/lib/amd64/server
EXPOSE 9077
CMD cd AceWiki; java -Xmx400m -Xss4m -Djava.library.path=/usr/lib/swi-prolog/lib/amd64/ -Djava.awt.headless=true -jar jetty-runner.jar --port 9077 --jar /usr/lib/swi-prolog/lib/jpl.jar acewiki.war
