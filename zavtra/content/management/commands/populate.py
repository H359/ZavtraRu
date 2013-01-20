# -*- coding: utf-8 -*-
from random import randint
from datetime import datetime, timedelta
from urllib import urlretrieve

from django.core.management.base import BaseCommand
from django.contrib.webdesign import lorem_ipsum
from django.core.files import File

from content.models import Rubric, Article
from siteuser.models import User


class Command(BaseCommand):

  def handle(self, *args, **kwargs):
    wod, cr = Rubric.objects.get_or_create(title=u'Слово Дня', slug='wod')
    if cr:
      wod.articles.all().delete()
    now = datetime.now()
    oneday = timedelta(days=1)
    start = now - 30 * oneday
    while True:
      article = Article.objects.create(
        rubric=wod,
        title=lorem_ipsum.words(1),
        subtitle=lorem_ipsum.words(randint(1,5)),
        status=Article.STATUS.ready,
        type=Article.TYPES.text,
        published_at=start,
        announce=lorem_ipsum.sentence(),
        content='<p>' + '</p><p>'.join(lorem_ipsum.paragraphs(randint(3,5))) + '</p>'
      )
      result = urlretrieve('http://placekitten.com/%d/%d' % (randint(700,800), randint(500,600)))
      article.cover_source.save('kote%d.jpg' % article.id, File(open(result[0])))
      article.save()
      start += oneday
      if start >= now:
        break