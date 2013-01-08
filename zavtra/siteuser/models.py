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
  mid_name = models.CharField(max_length=250, verbose_name=u'Отчество')
  last_name = models.CharField(max_length=250, verbose_name=u'Фамилия')
  level = models.IntegerField(choices=USER_LEVELS, default=USER_LEVELS.ordinary)

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