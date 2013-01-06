# -*- coding: utf-8 -*-
from datetime import datetime
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

from managers import PublishedArticlesManager, ZeitungManager, BlogsManager,\
                     NewsManager, ColumnsManager, WODManager, EditorialManager
from zavtra.utils import cached, oneday



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
    return u'%s %s' % ('-'*self.level, self.title)

  @property
  def zeitung_rubric(self):
    zeitung = Rubric.fetch_rubric('zeitung')
    return self.pk in map(lambda w: w.pk, zeitung)

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

  @models.permalink
  def get_absolute_url(self):
    return ('content.views.topic', (), {'slug': self.slug})


class Article(models.Model):
  STATUS = Choices(('draft', u'Черновик'), ('ready', u'Готово к публикации'))
  TYPES = Choices(('text', u'Текст'), ('video', u'Видео'))

  title = models.CharField(max_length=1024, verbose_name=u'Заголовок')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from=lambda i: u'%s-%s' % (i.title, i.published_at))
  subtitle = models.CharField(max_length=1024, verbose_name=u'Подзаголовок', blank=True)
  status = models.CharField(choices=STATUS, default=STATUS.draft, max_length=20)
  type = models.CharField(choices=TYPES, default=TYPES.text, max_length=20)
  published_at = models.DateTimeField(verbose_name=u'Время публикации')
  rubric = models.ForeignKey(Rubric, verbose_name=u'Рубрика', related_name='articles')
  authors = models.ManyToManyField(settings.AUTH_USER_MODEL, verbose_name=u'Авторы', blank=True)
  announce = models.TextField(verbose_name=u'Анонс (краткое содержание)')
  cover_source = models.ImageField(upload_to='articles/covers', verbose_name=u'Обложка')
  content = models.TextField(verbose_name=u'Текст', default='')
  topics = models.ManyToManyField(Topic, verbose_name=u'Темы', blank=True)

  # managers
  objects = PublishedArticlesManager()
  zeitung = ZeitungManager()
  blogs = BlogsManager()
  news = NewsManager()
  columns = ColumnsManager()
  editorial = EditorialManager()
  wod = WODManager()

  # old `objects` manager
  everything = models.Manager()

  # covers
  cover_for_main = ImageSpec([ResizeToFit(200, 120, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_wod = ImageSpec([ResizeToFit(428, 180, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_eventbox = ImageSpec([ResizeToFit(200, 200, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_list = ImageSpec([ResizeToFit(140, 128, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')

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
  def from_zeitung(self):
    return self.rubric.zeitung_rubric

  @property
  def issue_number(self):
    if self.from_zeitung:
      pt = self.published_at.date()
      npt = pt - oneday*(pt.weekday() + 5)
      if pt.weekday() >= 2:
        npt += 7*oneday
      return 1 + (npt - datetime(year=npt.year,day=1,month=1).date()).days / 7
    return None

  @property
  def issue_date(self):
    pt = self.published_at.date()
    npt = pt - oneday*(pt.weekday() + 5)
    if pt.weekday() >= 2:
      npt += 7*oneday
    return npt

  @property
  def other_authors_articles(self):
    return Article.objects.\
           filter(authors__in = self.authors.all()).\
           exclude(pk = self.pk)

  @property
  def other_issue_articles(self):
    pdate = self.published_at.date()
    return Article.zeitung.exclude(pk = self.pk).\
           filter(published_at__range = (pdate, pdate + oneday))

  @staticmethod
  def get_current_issue_date_range():
    now = datetime.now().date() - 14 * oneday # DEBUG
    wstart = now - oneday*(now.weekday() + 5)
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
    return Article.zeitung.\
           filter(published_at__range = Article.get_current_issue_date_range()).\
           order_by('rubric__position','-published_at')