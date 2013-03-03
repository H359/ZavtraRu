# -*- coding: utf-8 -*-
from django.db import models
from django.conf import settings

from article import Article


class ExpertComment(models.Model):
  expert = models.ForeignKey(settings.AUTH_USER_MODEL, verbose_name=u'Эксперт', related_name='expert_comments')
  article = models.ForeignKey(Article, verbose_name=u'Статья', related_name='expert_comments')
  comment = models.TextField(verbose_name=u'Текст')
  position = models.PositiveIntegerField(verbose_name=u'Позиция')

  def __unicode__(self):
    return u'%s' % self.expert

  class Meta:
    ordering = ['-position']
    verbose_name = u'Комментарий эксперта'
    verbose_name_plural = u'Комментарии экспертов'
    app_label = 'content'
