#! /usr/bin/env python

# AceWiki data converter (work in progress)
# Author: Kaarel Kaljurand
# Version: 2012-02-23
#
# This script provides the conversion of a given AceWiki data file
# into other formats, e.g. JSON and GF.
#
# Example:
#
# python convert_acewiki.py --in geo.acewikidata --format gfabs --name Geo > Geo.gf
# python convert_acewiki.py --in geo.acewikidata --format gfconc --name Geo > GeoEng.gf
#
import sys
import argparse
import os
import re
import time
import simplejson
from string import Template

pattern_sep = re.compile('^\s*$')
pattern_name = re.compile('^([0-9]+)$')
pattern_type = re.compile('^type:([a-z]+)$')
pattern_words = re.compile('^words:(.+)$')

# TODO: put these into a different module
gf = {}
gf['propername'] = ['PN', 'awPN']
gf['noun'] = ['CN', 'awCN']
gf['nounof'] = ['CN', 'awCNof']
gf['trverb'] = ['V2', 'awV2']
gf['tradj'] = ['A2', 'awA2']
gf['mainpage'] = ['Dummy', 'awDummy']

template_abs = Template("""
abstract ${name} = Attempto ** {

fun
""")

template_conc = Template("""
concrete ${name}Eng of $name = AttemptoEng **
  open SyntaxEng, ParadigmsEng, IrregEng, (C = ConstructX) in {

-- TODO: review these, maybe we can do better
-- Currently there is an extra 'by' with past participles
oper awCN : (_,_,_:Str) -> CN = \\sg,pl,d -> mkCN (ParadigmsEng.mkN sg pl) ;
oper awCNof : (_,_:Str) -> CN = \\x,d1 -> mkCN (ParadigmsEng.mkN x) ;
oper awPN : (_,_,_,_,_:Str) -> PN = \\x,d1,d2,d3,d4 -> mkPN x ;
oper awV2 : (_,_,_,_:Str) -> V2 = \\goes,go,gone,d -> mkV2 (ParadigmsEng.mkV go goes "dummy" gone "dummy") ;
oper awA2 : (_,_:Str) -> A2 = \\x,d1 -> mkA2 (mkA x) (mkPrep "") ;

lin""")


def parse_acewiki(path):
	"""
	Parses AceWiki data file into a Python data structure
	"""
	data = {}
	id = -1
	f = open(path, 'r')
	for line in f:
		line = line.strip()
		m = pattern_name.match(line)
		if m is not None:
			id = m.group(1)
			data[id] = {}
			continue
		m = pattern_type.match(line)
		if m is not None:
			data[id]['type'] = m.group(1)
			continue
		m = pattern_words.match(line)
		if m is not None:
			data[id]['words'] = m.group(1).split(';')
			continue
		# TODO: parse sentences and comments
	f.close()
	return data


def out_gf_abs(data, name):
	"""
	Outputs the GF abstract syntax file
	"""
	print template_abs.substitute(name = name)
	for id in data:
		if 'words' in data[id]:
			type = data[id]['type']
			[gf_type, gf_oper] = gf[type]
			print 'w{}_{} : {} ;'.format(id, gf_type, gf_type)
	print '}'


def gf_quote(str):
	"""
	Changes the underscore for a space and quotes the string.
	"""
	return '"' + str.replace("_", " ").replace('"', '\\"') + '"'


def out_gf_conc(data, name):
	"""
	Outputs the GF concrete syntax file
	"""
	print template_conc.substitute(name = name)
	for id in data:
		if 'words' in data[id]:
			words = ' '.join([gf_quote(x) for x in data[id]['words']])
			type = data[id]['type']
			[gf_type, gf_oper] = gf[type]
			print 'w{}_{} = {} {} ;'.format(id, gf_type, gf_oper, words)
	print '}'


parser = argparse.ArgumentParser(description='AceWiki data format converter')

parser.add_argument('-i', '--in', type=str, action='store', dest='file_in',
                   help='set the input file that contains AceWiki data (OBLIGATORY)')

parser.add_argument('-n', '--name', type=str, action='store', dest='name',
                   default="Acewiki",
                   help='set the name of the grammar (default: AceWiki)')

parser.add_argument('-f', '--format', type=str, action='store', dest='fmt',
                   default="json",
                   help='set the output format, one of {json} (default: json)')

parser.add_argument('-v', '--version', action='version', version='%(prog)s v0.1')

args = parser.parse_args()

# TODO: there is probably a better way to do this
if args.file_in is None:
	print >> sys.stderr, 'ERROR: argument -i/--in is not specified'
	exit()

data = parse_acewiki(args.file_in)

if args.fmt == "json":
	print simplejson.dumps(data)

if args.fmt == "gfabs":
	out_gf_abs(data, args.name)

if args.fmt == "gfconc":
	out_gf_conc(data, args.name)


print >> sys.stderr, 'Size: {}'.format(len(data))
