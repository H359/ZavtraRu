# -*- coding: utf-8 -*-
from datetime import datetime, timedelta
from django.db import models
from django.core.exceptions import ObjectDoesNotExist
from django.contrib.auth.models import AbstractBaseUser

from model_utils import Choices
from siteuser.managers import UserManager, ColumnistsManager
from imagekit.models import ImageSpec
from imagekit.processors.resize import ResizeToFit, ResizeToFill

from zavtra.utils import OpenGraphMixin


class User(OpenGraphMixin, AbstractBaseUser):
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
  photo_152 = ImageSpec([ResizeToFill(152, 152, 'c')], image_field='photo')
  photo_225 = ImageSpec([ResizeToFill(225, 169, 'b')], image_field='photo')

  def __unicode__(self):
    names = filter(lambda w: len(w) > 0, [self.first_name, self.last_name])
    return u' '.join(names)

  class Meta:
    ordering = ('last_name', 'first_name')

  @property
  def open_graph_data(self):
    yield ('og:type', 'profile')
    if self.photo != "":
      yield ('og:image', self.photo.url)
    yield ('og:title', unicode(self))
    yield ('og:description', self.resume)
    yield ('profile:first_name', self.first_name)
    yield ('profile:last_name', self.last_name)

  @property
  def is_staff(self):
    return self.level >= self.USER_LEVELS.staff

  def has_module_perms(self, app_label):
    # stub
    return self.is_staff

  def has_perm(self, permission):
    return self.is_staff

  @property
  def latest_article(self):
    if not hasattr(self, '__latest_article_cache'):
      thr = datetime.now() - timedelta(days=30)
      try:
        self.__latest_article_cache = self.articles.select_related().prefetch_related('topics').\
                                      filter(published_at__gte = thr).latest('published_at')
      except ObjectDoesNotExist:
        self.__latest_article_cache = None
    return self.__latest_article_cache

  @property
  def received_comments(self):
    from comments.models import Comment
    return Comment.enabled.filter(article__authors__in = [self.pk])

  @models.permalink
  def get_absolute_url(self):
    return ('siteuser.views.profile', (), {'pk': self.pk})

  @models.permalink
  def get_subscribe_url(self):
    return ('siteuser.views.subscribe', (), {'readee': self.pk})

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