#-*- coding: utf-8 -*-
from django.db import models
from django.contrib.auth.models import User

class Permission(models.Model):
    class Meta:
	verbose_name=u'Доступ'
	verbose_name_plural=u'Доступы'

class Role(models.Model):
    class Meta:
	verbose_name=u'Роль'
	verbose_name_plural=u'Роли'
    permissions = models.ManyToMany(Permission, verbose_name=u'Доступы')

class UserRole(models.Model):
    