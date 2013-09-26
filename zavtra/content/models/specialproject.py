# -*- coding: utf-8 -*-
from datetime import datetime

from django.db import models

from base import TitledSluggedModel
from article import Article

class SpecialProject(TitledSluggedModel):
  date = models.DateField(verbose_name=u'Дата', default=lambda: datetime.now())
  articles = models.ManyToManyField(Article, verbose_name=u'Статьи')

  class Meta:
    verbose_name = u'Спецпроект'
    verbose_name_plural = u'Спецпроекты'
    app_label = 'content'

  @models.permalink
  def get_absolute_url(self):
    return ('content.view.special_project', (), {'slug': self.slug})

  @staticmethod
  def get_current():
    res = None
    try:
      res = SpecialProject.objects.prefetch_related('articles').\
            filter(date__lte = datetime.now().date()).\
            latest('date')
    except SpecialProject.DoesNotExist:
      pass
    return res