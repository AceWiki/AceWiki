FROM ubuntu:22.04
WORKDIR /opt/application
RUN apt-get -y update
RUN apt-get install -y wget git ant
RUN apt-get install -y swi-prolog swi-prolog-java
RUN apt-get install -y openjdk-11-jdk
RUN git clone --depth=1 https://github.com/Attempto/APE.git
WORKDIR /opt/application/APE
RUN make build
WORKDIR /opt/application
RUN git clone --depth=1 https://github.com/AceWiki/AceWiki
WORKDIR /opt/application/AceWiki
COPY web.xml war/WEB-INF/web.xml
RUN ant createwebapps
RUN mv ../APE/ape.exe .
RUN wget -O jetty-runner.jar https://repo1.maven.org/maven2/org/mortbay/jetty/jetty-runner/8.1.9.v20130131/jetty-runner-8.1.9.v20130131.jar
ENV LD_PRELOAD /usr/lib/swi-prolog/lib/x86_64-linux/libjpl.so
ENV LD_LIBRARY_PATH /usr/lib/jvm/default-java/jre/lib/x86_64-linux:/usr/lib/jvm/default-java/lib/server
EXPOSE 9077
CMD java -Xmx400m -Xss4m -Djava.library.path=/usr/lib/swi-prolog/lib/x86_64-linux/ -Djava.awt.headless=true -jar jetty-runner.jar --port 9077 --jar /usr/lib/swi-prolog/lib/jpl.jar acewiki.war
