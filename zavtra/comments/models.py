# -*- coding: utf-8 -*-
from django.db import models
from django.conf import settings

from content.models import Article
from siteuser.models import User as UserModel
from managers import ActiveManager


class Comment(models.Model):
  article = models.ForeignKey(Article, verbose_name=u'Статья', related_name='comments')
  parent = models.ForeignKey('self', null=True, blank=True, related_name='children', verbose_name=u'Родитель')
  author = models.ForeignKey(settings.AUTH_USER_MODEL, verbose_name=u'Автор', related_name='comments')
  created_at = models.DateTimeField(auto_now_add=True, verbose_name=u'Дата создания')
  comment = models.TextField(verbose_name=u'Текст')
  active = models.BooleanField(verbose_name=u'Отображаемый', default=True)
  rating = models.IntegerField(verbose_name=u'Рейтинг', default=0)

  objects = models.Manager()
  enabled = ActiveManager()
