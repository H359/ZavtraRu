#-*- coding: utf-8 -*-
import sys, os, zipfile, subprocess, re, codecs

import pprint

from django.utils.encoding import force_unicode, smart_unicode
from django.core.exceptions import ObjectDoesNotExist
from django.core.management.base import BaseCommand
from django.conf import settings
from django.db import transaction

from corecontent.models import Article, User

wrapped_stdout = codecs.getwriter('UTF-8')(sys.stdout)
sys.stdout = wrapped_stdout

class Command(BaseCommand):
    PANDOC_PATH = '/home/zw0rk/.cabal/bin/pandoc'
    ARCHIVE_PATH = '/home/zw0rk/Work/zavtra_archive/data/zavtra/'
    agahidd = re.compile(ur'<agahidd[\n\r]*(Author: (?P<author>[^\n\r]+)[\n\r]*)?(Title: (?P<title>[^\n\r]+)[\n\r]*)?', re.I | re.U | re.M)
    pprinter = pprint.PrettyPrinter(indent=4)
    seps = re.compile(ur'[,—]')
    no_op = []
    titles = (
	u'генерал', u'полковник', u'адмирал', u'вице-адмирал', u'генерал-майор', u'л-т', u'капитан (I{1,3}|\d)+ ранга', u'генерал-полковник',
	u'профессор', u'академик', u'Священник'
    )
    _names = (
	ur'((?P<fname>[А-ЯёЁ]+\.)\s*(?P<lname>[А-ЯёЁ.]+))$',
	ur'((?P<title>%s)?\s*(?P<fname>[А-ЯёЁ.]+)\s+(?P<lname>[-А-ЯёЁ.]+))$' % u'|'.join(titles),
    )
    known_authors = {}
    link_re = re.compile(ur'(\[[^\]]+\]\((?P<url>[^)]+)\))', re.I | re.U | re.M)
    header_re = re.compile(ur'^(\*\*(?P<title>[^*]+?)\:?\*\*)$', re.I | re.U | re.M)
    junk_re = re.compile(ur'(?P<num>\d+(-я|\.)\s*,\s*)?\"?(?P<title>[А-ЯёЁ\s]+)\"?', re.I | re.U | re.M)
    def detect_authors(self, authors):
	res = []
	if authors is None:
	    return None
	authors = smart_unicode(authors)
	spl = self.seps.split(authors.strip())
	sps = []
	for x in spl:
	    for y in self.names:
		m = y.match(x)
		if m is not None:
		    z = m.groupdict()
		    p = (z['fname'], z['lname'])
		    if p not in self.known_authors:
			self.known_authors[p] = z
	return res
    def fix_markdown(self, data):
	def fix_line(line):
	    line = line.strip()
	    if line.endswith('\\'):
		line = line[0:-1]
	    line = line.strip()
	    if len(line) == 0:
		line = ''
	    return line
	return u'\r\n'.join(map(fix_line, data.split(u'\n')))
    def pandoc(self, data, ffrom, fto):
        p = subprocess.Popen(
	    [self.PANDOC_PATH, '--from=%s' % ffrom, '--to=%s' % fto],
	    stdin=subprocess.PIPE,
	    stdout=subprocess.PIPE
	)
	res = p.communicate(data)[0].decode('utf-8')
	if fto == 'markdown':
	    res = self.fix_markdown(res)
	return res
    def remove_junk(self, data):
	junk = self.junk_re.search(data)
	if junk:
	    data = junk.groupdict().get('title')
	return data
    def handle(self, *args, **kwargs):
	self.names = [re.compile(x, re.I | re.U | re.M) for x in reversed(sorted(self._names, key=len))]
	titles_agg = set([])
	for year_dir in os.listdir(self.ARCHIVE_PATH):
	    for issue in os.listdir(os.path.join(self.ARCHIVE_PATH, year_dir)):
		filepath = os.path.join(self.ARCHIVE_PATH, year_dir, issue)
		with zipfile.ZipFile(filepath, 'r') as issuezip:
		    issue_data = {}
		    for name in filter(lambda w: w.endswith('.html') and not w.endswith('gst.html'),  issuezip.namelist()):
			with issuezip.open(name, 'r') as article:
			    data = article.read().decode('windows-1251').encode('utf-8')
			    #clean_data = self.pandoc(data, 'html', 'markdown')
			    if name == 'index.html':
				clean_data = self.pandoc(data, 'html', 'markdown')
				diva = clean_data
				division = '------АРХИВНЫЙ ВЫПУСК------'
				for line in filter(lambda w: len(w) > 0, clean_data.split(u'\r\n')):
				    h = self.header_re.search(line)
				    if h:
					division = self.remove_junk(h.groupdict().get('title'))
				    else:
					m = self.link_re.search(line)
					if m:
					    issue_data.setdefault(division, [])
					    issue_data[division].append(m.groupdict().get('url'))
				#print self.pandoc(clean_data, 'markdown', 'html').decode('utf-8')
			    """
			    else:
				hidd = self.agahidd.search(data)
				if hidd is None:
				    self.no_op.append(os.path.join(filepath, name))
				else:
				    metadata = hidd.groupdict()
				    title = smart_unicode(metadata.get('title'))
				    authors = self.detect_authors(metadata.get('author'))
				    #issue_data[name] = {'metadata': metadata, 'text': u'', 'filepath': os.path.join(filepath, name)}
			    """
		"""
		print filepath
		for k,v in issue_data.items():
		    print k
		    for l in v:
			print u'\t %s' % l
		print ''
		print ''
		if len(issue_data.keys()) < 3:
		    print filepath
		    print diva
		    print ''
		    for k,v in issue_data.items():
			print smart_unicode(k)
			for l in v:
			    print u'\t %s' % l
		    print '\n'*3
		"""
		titles_agg.update(set(issue_data.keys()))
		#break
	    #break
	for x in titles_agg:
	    print x