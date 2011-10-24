#-*- coding: utf-8 -*-
from django.db import models
from django.contrib.auth.models import User

from annoying.fields import AutoOneToOneField

class UserProfile(models.Model):
    user      = AutoOneToOneField(User, primary_key=True)
    birthdate = models.DateField(verbose_name=u'Дата рождения', blank=True, null=True)
    avatar    = models.ImageField(upload_to='users/avatar', blank=True, null=True)
