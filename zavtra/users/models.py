# -*- coding: utf-8 -*-
from django.db import models
from django.contrib.auth.models import User as ContribUser

from utils.models import WithDenormalizedStats

class User(ContribUser, WithDenormalizedStats):
    dob          = models.DateField(verbose_name=u'Дата рождения')
    gender       = models.IntegerField(choices=GENDER, verbose_name=u'Пол')
    location     = models.CharField(max_length=100, verbose_name=u'Страна, город', blank=True)
    occupation   = models.CharField(max_length=200, verbose_name=u'Род занятий', blank=True)
    about        = models.TextField(blank=True, verbose_name=u'О себе')
    #avatar       = 

    #avatar_50    =
