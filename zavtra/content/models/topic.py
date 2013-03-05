# -*- coding: utf-8 -*-
from django.db import models

from base import TitledSluggedModel


class Topic(TitledSluggedModel):
  position = models.PositiveIntegerField(verbose_name=u'Позиция', default=0)
  on_top = models.BooleanField(verbose_name=u'Выводить в главном меню', default=False)

  def __unicode__(self):
    return u'%s' % self.title

  class Meta:
    ordering = ['position']
    verbose_name = u'Тема'
    verbose_name_plural = u'Темы'
    app_label = 'content'

  @models.permalink
  def get_absolute_url(self):
    return ('content.view.topic', (), {'slug': self.slug})

  @staticmethod
  def autocomplete_search_fields():
    return ("id__iexact", "title__icontains",)