#!/bin/sh

# On the basis of the AceWiki data file,
# list all the articles that are not referenced.
# References from comments are not checked however.
#
# Kaarel Kaljurand
# 2012-03-18

if [ $# -ne 1 ]
then
	echo "Usage: unreferenced.sh <wiki.acewikidata>"
	exit
fi

data=$1
aw_articles="aw_articles.txt"
aw_references="aw_references.txt"
aw_unreferenced="aw_unreferenced.txt"

cat $data | egrep "^[0-9]+$" > ${aw_articles}
cat $data | tr ' ' '\012' | egrep "^<[0-9]+,[0-9]+>$" | sed "s/,.*//" | sed "s/<//" | sort | uniq > ${aw_references}
fgrep -xvf ${aw_references} ${aw_articles} > ${aw_unreferenced}
egrep -A2 -xf ${aw_unreferenced} $data
