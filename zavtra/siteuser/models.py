# -*- coding: utf-8 -*-
from datetime import datetime
from django.db import models
from django.contrib.auth.models import AbstractBaseUser

from model_utils import Choices
from siteuser.managers import UserManager, ColumnistsManager
from imagekit.models import ImageSpec
from imagekit.processors.resize import ResizeToFit


class User(AbstractBaseUser):
  USER_LEVELS = Choices(
    (0, 'ordinary',  u'Обычный'),
    (1, 'trusted',   u'Доверенный'),
    (2, 'columnist', u'Колумнист'),
    (3, 'staff',     u'Сотрудник'),
  )

  email = models.EmailField(max_length=254, unique=True)  
  first_name = models.CharField(max_length=250, verbose_name=u'Имя')
  last_name = models.CharField(max_length=250, verbose_name=u'Фамилия')
  level = models.IntegerField(choices=USER_LEVELS, default=USER_LEVELS.ordinary)
  resume = models.CharField(max_length=1024, verbose_name=u'Короткое описание (регалии и т.п.)', blank=True)
  bio = models.TextField(verbose_name=u'Биография', blank=True)
  photo = models.ImageField(verbose_name=u'Фотография', blank=True, null=True, upload_to='authors')
  date_joined = models.DateTimeField(verbose_name=u'Дата регистрации', default=lambda: datetime.now())

  USERNAME_FIELD = 'email'

  objects = UserManager()
  columnists = ColumnistsManager()

  photo_90 = ImageSpec([ResizeToFit(90, 90, True, 0xFFFFFF)], image_field='photo')
  photo_60 = ImageSpec([ResizeToFit(60, 60, True, 0xFFFFFF)], image_field='photo')

  @property
  def is_staff(self):
    return self.level >= self.USER_LEVELS.staff

  def has_module_perms(self, app_label):
    # stub
    return self.is_staff

  def has_perm(self, permission):
    return self.is_staff

  def __unicode__(self):
    names = filter(lambda w: len(w) > 0, [self.first_name, self.last_name])
    return u' '.join(names)

  @property
  def received_comments(self):
    from comments.models import Comment
    return Comment.enabled.filter(article__authors__in = [self.pk])

  @models.permalink
  def get_absolute_url(self):
    return ('siteuser.views.profile', (), {'pk': self.pk})

  @models.permalink
  def get_articles_url(self):
    return ('siteuser.views.profile_articles', (), {'pk': self.pk})

  @models.permalink
  def get_comments_url(self):
    return ('siteuser.views.profile_comments', (), {'pk': self.pk})    

  @staticmethod
  def autocomplete_search_fields():
    return ("id__iexact", "last_name__icontains",)


class Reader(models.Model):
  reader = models.ForeignKey(User, related_name='readers')
  author = models.ForeignKey(User, related_name='readees')
  subscription_start = models.DateTimeField()