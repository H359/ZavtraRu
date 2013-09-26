#-*- coding: utf-8 -*-
from django.db import models
from article import Article


class ExtractedQuote(models.Model):
  source     = models.ForeignKey(Article, verbose_name=u'Источник')
  quote      = models.CharField(max_length=200, verbose_name=u'Текст цитаты')
  created_at = models.DateTimeField(auto_now=True)

  class Meta:
    app_label = 'content'
    verbose_name = u'Цитата'
    verbose_name_plural = u'Цитаты'
    ordering = ('-created_at',)