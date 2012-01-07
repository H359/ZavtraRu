#-*- coding: utf-8 -*-
import re
import xml.parsers.expat

from django.utils.encoding import force_unicode
from django.core.exceptions import ObjectDoesNotExist, MultipleObjectsReturned
from django.core.management.base import BaseCommand
from django.db import transaction

from corecontent.models import User, ContentItem, Rubric

class ArticlesParser(object):
    users = {}
    state = []

    authors = []
    text = u""
    title = u""
    pubdate = 0
    url = u""

    dgt = re.compile('[0-9]+')

    authors_map = {}
    rubrics_map = {}

    parser = None

    def __init__(self):
	self.parser = xml.parsers.expat.ParserCreate()
	self.parser.StartElementHandler = self.start_element
	self.parser.EndElementHandler = self.end_element
	self.parser.CharacterDataHandler = self.char_data
	for i in range(1, 9):
	    self.rubrics_map[str(i)] = Rubric.objects.get_or_create(title=u'%d-я полоса [архив]' % i)[0]
    def reset(self):
	self.authors = []
	self.text = u""
	self.title = u""
	self.pubdate = 0
	self.url = u""
    def parse(self, f):
	self.parser.ParseFile(f)
    def correct_author(self, author):
	auth = author.split()
	if len(auth) != 2:
	    return None
	fname, lname = auth
	if not (fname[0].isupper() and lname[0].isupper()):
	    return None
	return (fname, lname)
    def correct_authors(self):
	self.authors = filter(lambda w: w is not None, map(self.correct_author, self.authors))
    def get_users(self, author):
	user = self.authors_map.get(author)
	if user is None:
	    try:
		user = User.objects.get(first_name=author[0], last_name=author[1], is_staff=True)
	    except ObjectDoesNotExist:
		username = 'ext_%d' % (len(self.authors_map.keys())+1)
		user = User(username=username)
		user.is_staff = True
		user.is_active = False
		user.first_name = author[0]
		user.last_name = author[1]
		user.set_unusable_password()
		user.save()
	    except MultipleObjectsReturned:
		user = None
	    if user is not None:
		self.authors_map[author] = user
	return user
    def publish(self):
	spl = self.url.split('.')
	print self.pubdate, self.title, self.url, spl
	t = self.dgt.search(spl[0]).group()
	rubric = self.rubrics_map[t[0]]
	users = filter(lambda w: w is not None, map(self.get_users, self.authors))
	self.title = self.title.strip()
	if len(self.title) == 0:
	    self.title = t
	omit = False
	nt = []
	for x in self.title:
	    if x == u'(': omit = True
	    if not omit: nt.append(x)
	    if x == u')': omit = False
	self.title = u''.join(nt)
	self.title = self.title[0:249]
	cti = ContentItem(
	    title = self.title,
	    slug = self.pubdate + '_' + spl[0],
	    rubric = rubric,
	    pub_date = self.pubdate,
	    published=True,
	    enabled=True,
	    kind='text',
	    content=self.text,
	    old_url=self.url
	)
	cti.save(notypo=True)
	for u in users:
	    cti.authors.add(u)
    def start_element(self, name, attrs):
	if 'pubdate' in attrs: self.pubdate = attrs.get('pubdate')
	self.state.append(name)
    def end_element(self, name):
	state = self.state.pop()
	if state != name:
	    raise Exception, u'Invalid XML'
	if state == 'article':
	    self.correct_authors()
	    with transaction.commit_on_success():
		self.publish()
	    self.reset()
    def char_data(self, data):
	state = self.state[-1]
	if state == 'author':
	    self.authors.append(data)
	elif state == 'title':
	    self.title += data
	elif state == 'text':
	    self.text += data
	elif state == 'url':
	    self.url += data

class Command(BaseCommand):
    def handle(self, *args, **kwargs):
	print 'DISABLED!'
	return
	a = ArticlesParser()
	ContentItem.objects.filter(rubric__title__regex = u'[0-9]+-я').delete()
	User.objects.filter(username__regex = u'^ext_[0-9]+$', is_staff=True, is_active=False).delete()
	with open('data.xml', 'r') as fxml:
	    a.parse(fxml)