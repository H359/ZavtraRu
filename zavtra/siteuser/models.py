#-*- coding: utf-8 -*-
import urllib, md5

from django.db import models
from django.contrib.auth.models import User

default_avatar = 'http://zavtra.com/static/img/anon.jpg'

def email2avatar(email):
    return u'http://www.gravatar.com/avatar/%s?%s' % (
        md5.new(email.lower()).hexdigest(),
        urllib.urlencode({'d': default_avatar, 's': str(48)})
    )

class SiteProfile(models.Model):
    GENDER = (
	(1, u'Мужской'),
	(2, u'Женский'),
    )
    user       = models.OneToOneField(User)
    dob        = models.DateField(verbose_name=u'Дата рождения')
    gender     = models.IntegerField(choices=GENDER, verbose_name=u'Пол')
    location   = models.CharField(max_length=100, verbose_name=u'Страна, город', blank=True)
    occupation = models.CharField(max_length=200, verbose_name=u'Род занятий', blank=True)
    about      = models.TextField(blank=True, verbose_name=u'О себе')
    rating     = models.IntegerField(default=0)
    can_blog   = models.BooleanField(verbose_name=u'Может писать в блоги', default=False)
    activation = models.CharField(max_length=32, editable=False, verbose_name=u'Код активации')

    @property
    def avatar(self):
	return self.user.avatar()

setattr(User, 'avatar', lambda s: email2avatar(s.email))