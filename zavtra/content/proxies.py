#-*- coding: utf-8 -*-
from content.models import Article

# Can't use dedicated managers here because of their published_at constraint.

class News(Article):
  class Meta:
    proxy = True
    verbose_name = u'Новость'
    verbose_name_plural = u'Новости'


class Wod(Article):
  class Meta:
    proxy = True
    verbose_name = u'Слово Дня'
    verbose_name_plural = u'Слова Дня'


class Video(Article):
  class Meta:
    proxy = True
    verbose_name = u'Видео'
    verbose_name_plural = u'Видео'
