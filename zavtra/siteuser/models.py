# -*- coding: utf-8 -*-
from django.db import models
from django.contrib.auth.models import AbstractBaseUser

from model_utils import Choices
from siteuser.managers import UserManager, ColumnistsManager


class User(AbstractBaseUser):
  USER_LEVELS = Choices(
    (0, 'ordinary',  u'Обычный'),
    (1, 'trusted',   u'Доверенный'),
    (2, 'columnist', u'Колумнист'),
    (3, 'staff',     u'Сотрудник'),
  )

  email = models.EmailField(max_length=254, unique=True)  
  first_name = models.CharField(max_length=250, verbose_name=u'Имя')
  mid_name = models.CharField(max_length=250, verbose_name=u'Отчество', blank=True)
  last_name = models.CharField(max_length=250, verbose_name=u'Фамилия')
  level = models.IntegerField(choices=USER_LEVELS, default=USER_LEVELS.ordinary)
  resume = models.CharField(max_length=1024, verbose_name=u'Короткое описание (регалии и т.п.)', blank=True)
  bio = models.TextField(verbose_name=u'Биография', blank=True)
  photo = models.ImageField(verbose_name=u'Фотография', blank=True, null=True, upload_to='authors')

  USERNAME_FIELD = 'email'

  objects = UserManager()
  columnists = ColumnistsManager()

  @property
  def is_staff(self):
    return self.level >= self.USER_LEVELS.staff

  def has_module_perms(self, app_label):
    # stub
    return self.is_staff

  def has_perm(self, permission):
    return self.is_staff

  def __unicode__(self):
    names = filter(lambda w: len(w) > 0, [self.first_name, self.mid_name, self.last_name])
    return u' '.join(names)

  @models.permalink
  def get_absolute_url(self):
    return ('siteuser.views.profile', (), {'id': self.pk})

  @staticmethod
  def autocomplete_search_fields():
    return ("id__iexact", "last_name__icontains",)