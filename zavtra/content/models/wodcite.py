# -*- coding: utf-8 -*-
from django.db import models

from article import Article


class WodCite(models.Model):
  article = models.ForeignKey(Article, verbose_name=u'Статья', related_name='cites')
  #word = models.CharField(verbose_name=u'Слово', max_length=256)
  cite = models.TextField(verbose_name=u'Значение')
  source = models.CharField(verbose_name=u'Источник', max_length=1024)

  def __unicode__(self):
    return self.article.title

  class Meta:
    verbose_name = u'Выдержка из словаря'
    verbose_name_plural = u'Выдержки из словарей'
    app_label = 'content'
