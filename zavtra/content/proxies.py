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


class Column(Article):
  class Meta:
    proxy = True
    verbose_name = u'Колонка колумниста'
    verbose_name_plural = u'Колонки колумнистов'


class Editorial(Article):
  class Meta:
    proxy = True
    verbose_name = u'Объявление в колонке редактора'
    verbose_name_plural = u'Объявления в колонке редактора'


class Announcement(Article):
  class Meta:
    proxy = True
    verbose_name = u'Объявление'
    verbose_name_plural = u'Объявления'