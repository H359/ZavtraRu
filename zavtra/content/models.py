# -*- coding: utf-8 -*-
from datetime import datetime, timedelta
from pytils.translit import slugify

from django.db import models
from django.conf import settings

from mptt.models import MPTTModel, TreeForeignKey
from model_utils import Choices
from djorm_pgfulltext.models import SearchManager
from djorm_pgfulltext.fields import VectorField
from autoslug import AutoSlugField
from imagekit.models import ImageSpec
from imagekit.processors.resize import ResizeToFit

from managers import PublishedArticlesManager, GazetteManager, BlogsManager,\
                     NewsManager, ColumnsManager, WODManager
from zavtra.utils import cached


class ZhivotovIllustration(models.Model):
  image = models.ImageField(upload_to='zhivotov', verbose_name=u'Изображение')
  published_at = models.DateTimeField(verbose_name=u'Время публикации')

  gazette_box = ImageSpec([ResizeToFit(278, 121, True)], image_field='image', format='JPEG')

  class Meta:
    ordering = ['-published_at']
    verbose_name = u'Иллюстрация Животова'
    verbose_name_plural = u'Иллюстрации Животова'


class Rubric(MPTTModel):
  parent = TreeForeignKey('self', null=True, blank=True, related_name='children', verbose_name=u'Родитель')
  title = models.CharField(max_length=1024, verbose_name=u'Название')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from='title')
  position = models.PositiveIntegerField(verbose_name=u'Позиция', default=1)

  class MPTTMeta:
    order_insertion_by = ['-position']

  class Meta:
    ordering = ['-position']
    verbose_name = u'Рубрика'
    verbose_name_plural = u'Рубрики'

  @models.permalink
  def get_absolute_url(self):
    return ('content.views.rubric', (), {'slug': self.slug})

  def __unicode__(self):
    return u'%s' % self.title

  @staticmethod
  def fetch_rubric(slug):
    def get_rubric():
      return Rubric.objects.get(slug=slug).get_descendants(include_self = True)
    return cached(get_rubric, 'fetch-rubric-%s' % slug)


class Topic(models.Model):
  title = models.CharField(max_length=1024, verbose_name=u'Название')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from='title')
  position = models.PositiveIntegerField(verbose_name=u'Позиция', default=0)  
  on_top = models.BooleanField(verbose_name=u'Выводить в главном меню', default=False)

  class Meta:
    ordering = ['-position']
    verbose_name = u'Тема'
    verbose_name_plural = u'Темы'

  def __unicode__(self):
    return u'%s' % self.title


class Article(models.Model):
  STATUS = Choices(('draft', u'Черновик'), ('ready', u'Готово к публикации'))
  TYPES = Choices(('text', u'Текст'), ('video', u'Видео'))

  title = models.CharField(max_length=1024, verbose_name=u'Заголовок')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from=lambda i: u'%s-%s' % (i.title, i.published_at))
  subtitle = models.CharField(max_length=1024, verbose_name=u'Подзаголовок', blank=True)
  status = models.CharField(choices=STATUS, default=STATUS.draft, max_length=20)
  type = models.CharField(choices=TYPES, default=TYPES.text, max_length=20)
  published_at = models.DateTimeField(verbose_name=u'Время публикации')
  rubric = models.ForeignKey(Rubric, verbose_name=u'Рубрика')
  authors = models.ManyToManyField(settings.AUTH_USER_MODEL, verbose_name=u'Авторы')
  announce = models.TextField(verbose_name=u'Анонс (краткое содержание)')
  cover_source = models.ImageField(upload_to='articles/covers', verbose_name=u'Обложка')
  content = models.TextField(verbose_name=u'Текст', default='')

  # managers
  objects = PublishedArticlesManager()
  gazette = GazetteManager()
  blogs = BlogsManager()
  news = NewsManager()
  columns = ColumnsManager()
  wod = WODManager()

  everything = models.Manager()

  # Covers
  cover_for_main = ImageSpec([ResizeToFit(200, 120, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_dayheadline = ImageSpec([ResizeToFit(428, 180, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_eventbox = ImageSpec([ResizeToFit(200, 200, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')

  class Meta:
    ordering = ['-published_at']
    verbose_name = u'Статья'
    verbose_name_plural = u'Статьи'

  @models.permalink
  def get_absolute_url(self):
    return ('content.views.article', (), {'slug': self.slug})

  def __unicode__(self):
    return u'%s' % self.title

  @property
  def other_authors_articles(self):
    return Article.objects.\
           filter(authors__in = self.authors.all()).\
           exclude(pk = self.pk)

  @staticmethod
  def get_current_issue_date_range():
    oneday = timedelta(days=1)
    now = datetime.now().date() - timedelta(days=14) # DEBUG
    wstart = now - oneday*(now.weekday()+5)
    if now.weekday() >= 2:
      wstart += 7*oneday
    wend = wstart + 7*oneday
    return (wstart, wend)

  @staticmethod
  def get_current_issue_number():
    wstart, wend = Article.get_current_issue_date_range()
    return 1 + (wstart - datetime(year=wstart.year,day=1,month=1).date()).days / 7

  @staticmethod
  def get_current_issue():
    return Article.gazette.\
           filter(published_at__range = Article.get_current_issue_date_range()).\
           order_by('rubric__position','-published_at')