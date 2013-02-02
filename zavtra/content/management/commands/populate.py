# -*- coding: utf-8 -*-
from random import randint, choice
from datetime import datetime, timedelta
from urllib import urlretrieve

from django.core.management.base import BaseCommand
from django.contrib.webdesign import lorem_ipsum
from django.core.files import File

from content.models import Rubric, Article, ExpertComment
from siteuser.models import User


class Command(BaseCommand):
  def handle(self, *args, **kwargs):
    wod, _ = Rubric.objects.get_or_create(title=u'Слово дня', slug='wod')
    columnists, _ = Rubric.objects.get_or_create(title=u'Колумнисты', slug='columnists')
    news, _ = Rubric.objects.get_or_create(title=u'Новости', slug='novosti')
    now = datetime.now()
    oneday = timedelta(days=1)
    start = now - 3 * oneday
    pic_formats = [(x, (4 / 3) * x) for x in range(400, 900, 100)]
    while True:
      for p in [columnists, news, wod]:
        for x in range(0, 7):
          article = Article.objects.create(
            rubric=p,
            title=lorem_ipsum.words(randint(1,3)),
            subtitle=lorem_ipsum.words(randint(1,5)),
            status=Article.STATUS.ready,
            type=Article.TYPES.text,
            published_at=start,
            selected_at=start,
            announce=lorem_ipsum.sentence(),
            content='<p>' + '</p><p>'.join(lorem_ipsum.paragraphs(randint(3,5))) + '</p>'          
          )
          result = urlretrieve('http://placekitten.com/%d/%d' % choice(pic_formats))
          if p.id != news.id or (randint(0,10) > 4):
            article.cover_source.save('kote%d.jpg' % article.id, File(open(result[0])))
            article.save()
          if p.id == columnists.id:
            article.authors.add(User.columnists.all()[randint(1,2000)])
          elif p.id == wod.id:
            for x in range(0, randint(3,5)):
              ExpertComment.objects.create(
                expert = User.columnists.all()[randint(1,2000)],
                article = article,
                comment = lorem_ipsum.paragraph(),
                position = x + 1
              )
            break

      start += oneday
      if start >= now:
        break