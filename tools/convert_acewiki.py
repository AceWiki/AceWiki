#! /usr/bin/env python

# AceWiki data converter
# Author: Kaarel Kaljurand
# Version: 2012-02-23
#
# This script provides the conversion of a given AceWiki data file
# into other formats, e.g. JSON and GF.
#
# Examples:
#
# python convert_acewiki.py --in geo.acewikidata > Geo.json
# python convert_acewiki.py --in geo.acewikidata --format gfabs --name Geo > Geo.gf
# python convert_acewiki.py --in geo.acewikidata --format gfconc --name Geo > GeoEng.gf
# python convert_acewiki.py --in geo.acewikidata --format sentences > Geo.ace.txt
#
import sys
import argparse
import os
import re
import time
import simplejson
from string import Template

# Regular expression patterns
pattern_sep = re.compile('^\s*$')
pattern_name = re.compile('^([0-9]+)$')
pattern_type = re.compile('^type:([a-z]+)$')
pattern_words = re.compile('^words:(.+);$')
pattern_sentence = re.compile('^(c|\||#) (.+)$')
pattern_token = re.compile('^<([0-9]+),([0-9]+)>$')

# GF-specific templates and strings
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
-- We use ~ as a dummy symbol which represents all the forms
-- that we do not want to actually generate.
-- This seems to be less confusing that using an empty string.
oper awCN : (_,_:Str) -> CN = \\sg,pl -> mkCN (ParadigmsEng.mkN sg pl) ;
oper awCNof : (_:Str) -> CN = \\x -> mkCN (ParadigmsEng.mkN x "~") ;
oper awPN : (_,_,_,_:Str) -> PN = \\x,d1,d2,d3 -> mkPN x ;
oper awV2 : (_,_,_:Str) -> V2 = \\goes,go,gone -> mkV2 (ParadigmsEng.mkV go goes "~" gone "~") ;
oper awA2 : (_:Str) -> A2 = \\x -> mkA2 (mkA x) (mkPrep "") ;

lin""")


def parse_acewiki(path):
	"""
	Parses AceWiki data file into a Python data structure.
	TODO: Currently does not change the token representation (i.e. <id1,id2>).
	Assumes that article IDs are integers.
	This way we get an ordering for articles which results in a stable output.
	"""
	data = {}
	id = -1
	f = open(path, 'r')
	for line in f:
		line = line.strip()
		m = pattern_name.match(line)
		if m is not None:
			id = int(m.group(1))
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
		m = pattern_sentence.match(line)
		if m is not None:
			if 'sentences' not in data[id]:
				data[id]['sentences'] = []
			data[id]['sentences'].append({ 'type': m.group(1), 'content' : m.group(2).split(' ') })
			continue
	f.close()
	return data


def out_gf_abs(data, name):
	"""
	Outputs the data as GF abstract syntax file with the given name.
	"""
	print template_abs.substitute(name = name)
	for id in sorted(data):
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


def rewrite_words(type, words):
	"""
	Removes 'by' from past participles ('trverb').
	Removes 'of' from of-constructs ('nounof').
	This is currently needed only for the GF output.
	"""
	if type == 'trverb':
		[sg, pl, vbg] = words
		vbg = re.sub(r' by$', '', vbg)
		return [sg, pl, vbg]
	elif type == 'nounof':
		[nounof] = words
		return [re.sub(r' of$', '', nounof)]
	return words


def rewrite_token(data, token):
	"""
	If the given token is a function word then
	returns it lowercased (unless it is a variable),
	otherwise
	returns the wordform that corresponds to the ID-representation.
	"""
	m = pattern_token.match(token)
	if m is None:
		if token in ['X', 'Y', 'Z']:
			return token
		return token.lower()
	else:
		try:
			article_id = int(m.group(1))
			wordform_id = int(m.group(2))
			return data[article_id]['words'][wordform_id].replace('_', ' ')
		except:
			print >> sys.stderr, 'Warning: Bad token ID: {}'.format(token)
			return token


def out_gf_conc(data, name):
	"""
	Outputs the data as GF concrete syntax file with the given name.
	"""
	print template_conc.substitute(name = name)
	for id in sorted(data):
		if 'words' in data[id]:
			type = data[id]['type']
			words = rewrite_words(type, data[id]['words'])
			words_as_str = ' '.join([gf_quote(x) for x in words])
			[gf_type, gf_oper] = gf[type]
			print 'w{}_{} = {} {} ;'.format(id, gf_type, gf_oper, words_as_str)
	print '}'


def out_sentences(data):
	"""
	Outputs the data as a list of sentences (excl. comments).
	The token IDs are resolved to the wordforms.
	"""
	for id in sorted(data):
		if 'sentences' in data[id]:
			for s in data[id]['sentences']:
				if s['type'] == 'c':
					continue
				print ' '.join([rewrite_token(data, x) for x in s['content']])


# Commandline arguments parsing
parser = argparse.ArgumentParser(description='AceWiki data file converter')

parser.add_argument('-i', '--in', type=str, action='store', dest='file_in',
                   help='file that contains AceWiki data (OBLIGATORY)')

parser.add_argument('-n', '--name', type=str, action='store', dest='name',
                   default="Acewiki",
                   help='name of the grammar, used for GF outputs (default: AceWiki)')

parser.add_argument('-f', '--format', type=str, action='store', dest='fmt',
                   default="json",
                   help='output format, one of {json,gfabs,gfconc,sentences} (default: json)')

parser.add_argument('-v', '--version', action='version', version='%(prog)s v0.1')

args = parser.parse_args()

# TODO: there is probably a better way to do this
if args.file_in is None:
	print >> sys.stderr, 'ERROR: argument -i/--in is not specified'
	exit()

data = parse_acewiki(args.file_in)

if args.fmt == "json":
	print simplejson.dumps(data)
elif args.fmt == "gfabs":
	out_gf_abs(data, args.name)
elif args.fmt == "gfconc":
	out_gf_conc(data, args.name)
elif args.fmt == "sentences":
	out_sentences(data)
else:
	print >> sys.stderr, 'ERROR: Unsupported format: {}'.format(args.fmt)
