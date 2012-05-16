# -*- coding: utf-8 -*-
from datetime import datetime, timedelta
from random import randint, choice

from pytils.translit import translify

from django.contrib.auth.models import User
from django.core.management.base import BaseCommand
from django.contrib.webdesign.lorem_ipsum import sentence, paragraphs, words

from articles.models import Rubric, Article, ArticlePart, MarkdownText, YoutubeVideo

FNAMES = [u'Иван', u'Сергей', u'Феофан', u'Федот', u'Тихон', u'Михаил', u'Родомысл']
LNAMES = [u'Иванов', u'Петров', u'Сидоров', u'Кукушкин', u'Тетерин', u'Фадеев', u'Митькин']
VIDEO_URLS = [
	'http://www.youtube.com/watch?v=Fok1KUhjAKs', 'http://www.youtube.com/watch?v=NDMuQsZcsIo', 'http://www.youtube.com/watch?v=O-liDyNO4Tg',
	'http://www.youtube.com/watch?v=Nkl3vdl4AII', 'http://www.youtube.com/watch?v=w11m-Hvf6Po', 'http://www.youtube.com/watch?v=BSddP7rNTHM',
	'http://www.youtube.com/watch?v=mCRq-2acf-c', 'http://www.youtube.com/watch?v=mCRq-2acf-c', 'http://www.youtube.com/watch?v=may7ByX35AY'
]

def true_or_false():
	return randint(1,10) > 5

class Command(BaseCommand):
	rubrics = []
	authors = []
	
	def create_rubrics(self):
		self.rubrics = [Rubric.objects.create(title=words(count=randint(2,4))) for _ in range(randint(20,30))]
		self.rubrics.append(Rubric.objects.create(title=u'Новости'))
	
	def create_authors(self):
		defargs = {'is_staff': False, 'is_active': False, 'is_superuser': False, 'password': '!'}
		for fname in FNAMES:
		    for lname in LNAMES:
			user = User(**defargs)
			user.first_name = fname
			user.last_name  = lname
			user.username   = u'%s_%s' % (translify(unicode(user.first_name)), translify(unicode(user.last_name)))
			user.email      = u'%s@zavtra.ru' % user.username
			user.save()
			self.authors.append(user)
	
	def create_part_object(self):
		if randint(1,2) == 1:
			part = MarkdownText.objects.create(text='\n\n'.join(paragraphs(count=randint(2,5))))
		else:
			part = YoutubeVideo.objects.create(url=choice(VIDEO_URLS))
		return part
	
	def create_articles(self):
		oneday = timedelta(days=1)
		now    = datetime.now()
		start  = now.replace(month=now.month-1)
		while start < now:
			defargs = {'pub_date': start, 'enabled': True} 
			for _ in range(randint(10,20)):
				article = Article(**defargs)
				article.rubric = choice(self.rubrics)
				article.title=words(count=randint(2,5))
				article.published = true_or_false()
				article.exclusive = true_or_false()
				article.announce = sentence()
				article.save()
				for position in range(randint(1,4)):
					partObject = self.create_part_object()
					ArticlePart.objects.create(article=article, position=position, content_object=partObject)
			start += oneday
	
	def clean(self):
		Rubric.objects.all().delete()
		User.objects.all().delete()
		Article.objects.all().delete()
		ArticlePart.objects.all().delete()
	
	def handle(self, *args, **kwargs):
		print 'Cleaning DB...'
		self.clean()
		print 'Creating rubrics...'
		self.create_rubrics()
		print 'Creating authors...'
		self.create_authors()
		print 'Creating articles...'
		self.create_articles()