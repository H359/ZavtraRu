# -*- coding: utf-8 -*-
from datetime import datetime
from django.db import models

from article import Article


class DailyQuote(models.Model):
  quote = models.TextField(verbose_name=u'Цитата')
  source = models.ForeignKey(Article, verbose_name=u'Источник цитаты')
  day = models.DateField(verbose_name=u'День', unique=True, default=lambda: datetime.now())

  class Meta:
    ordering = ['-day']
    verbose_name = u'Цитата дня'
    verbose_name_plural = u'Цитаты дня'
    app_label = 'content'

  @staticmethod
  def get_current():
    try:
      return DailyQuote.objects.select_related().get(day=datetime.now().date())
    except DailyQuote.DoesNotExist:
      return None