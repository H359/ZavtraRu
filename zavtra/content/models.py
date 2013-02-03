# -*- coding: utf-8 -*-
from datetime import datetime
from pytils.translit import slugify
from urlparse import urlparse, parse_qs

from django.db import models
from django.db.models import Count
from django.conf import settings

from model_utils import Choices
from djorm_pgfulltext.models import SearchManager
from djorm_pgfulltext.fields import VectorField
from autoslug import AutoSlugField
from imagekit.models import ImageSpec
from imagekit.processors.resize import ResizeToFit

from siteuser.models import User as UserModel

from managers import PublishedManager, EventsManager, WODManager,\
                     NewsManager, BlogsManager
from zavtra.utils import cached, oneday



class Rubric(models.Model):
  title = models.CharField(max_length=1024, verbose_name=u'Название')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from='title')

  class Meta:
    verbose_name = u'Рубрика'
    verbose_name_plural = u'Рубрики'

  @staticmethod
  def fetch_rubric(slug):
    return cached(lambda: Rubric.objects.get(slug=slug), 'rubric:%s' % slug)

  @staticmethod
  def get_gazette_rubrics():
    # TODO: optimize this
    rii = RubricInIssue.objects.distinct('rubric').\
          values_list('rubric', flat=True).order_by('rubric', 'position')
    return Rubric.objects.filter(pk__in = rii)

  @models.permalink
  def get_absolute_url(self):
    return ('content.views.rubric', (), {'slug': self.slug})

  def __unicode__(self):
    return u'%s' % self.title


class Issue(models.Model):
  absolute_number = models.PositiveIntegerField(verbose_name=u'Номер (абсолютный)')
  relative_number = models.PositiveIntegerField(verbose_name=u'Номер (относительный)')
  published_at = models.DateField(verbose_name=u'Дата выхода')
  illustration = models.ImageField(upload_to='zhivotov', verbose_name=u'Иллюстрация Животова')

  objects = models.Manager()
  published = PublishedManager()

  gazette_box = ImageSpec([ResizeToFit(278, 121, True)], image_field='illustration', format='JPEG')
  zeitung_box = ImageSpec([ResizeToFit(870, 385, True)], image_field='illustration', format='JPEG')
  archive_box = ImageSpec([ResizeToFit(570, 252, True)], image_field='illustration', format='JPEG')

  class Meta:
    ordering = ['-published_at']
    verbose_name = u'Выпуск'
    verbose_name_plural = u'Выпуск'

  @property
  def gazette(self):
    # TODO: cache?
    issue_rubrics = list(self.issue_rubrics.select_related().all())
    articles = Article.objects.filter(
      published_at__year = self.published_at.year,
      published_at__month = self.published_at.month,
      published_at__day = self.published_at.day,
      rubric__in = [x.rubric for x in issue_rubrics]
    ).prefetch_related('authors').select_related()
    rubric_positions = {r.rubric_id: r.position for r in issue_rubrics}
    return sorted(articles, key=lambda a: rubric_positions[a.rubric_id])

  @models.permalink
  def get_absolute_url(self):
    kwargs = {
      'year': self.published_at.year,
      'issue': self.relative_number
    }
    return ('content.views.zeitung', (), kwargs)


class RubricInIssue(models.Model):
  issue = models.ForeignKey(Issue, verbose_name=u'Выпуск', related_name='issue_rubrics')
  rubric = models.ForeignKey(Rubric, verbose_name=u'Рубрика')
  position = models.PositiveIntegerField(verbose_name=u'Позиция')

  class Meta:
    ordering = ['position']
    verbose_name = u'Рубрика в выпуске'
    verbose_name_plural = u'Рубрика в выпуске'

  def __unicode__(self):
    return u'%s' % self.rubric


class Topic(models.Model):
  title = models.CharField(max_length=1024, verbose_name=u'Название')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from='title')
  position = models.PositiveIntegerField(verbose_name=u'Позиция', default=0)
  on_top = models.BooleanField(verbose_name=u'Выводить в главном меню', default=False)

  class Meta:
    ordering = ['-position']
    verbose_name = u'Тема'
    verbose_name_plural = u'Темы'

  @models.permalink
  def get_absolute_url(self):
    return ('content.views.topic', (), {'slug': self.slug})

  def __unicode__(self):
    return u'%s' % self.title

  @staticmethod
  def autocomplete_search_fields():
    return ("id__iexact", "title__icontains",)


class Article(models.Model):
  STATUS = Choices(('draft', u'Черновик'), ('ready', u'Готово к публикации'))
  TYPES = Choices(('text', u'Текст'), ('video', u'Видео'))

  rubric = models.ForeignKey(Rubric, verbose_name=u'Рубрика', related_name='articles')
  title = models.CharField(max_length=1024, verbose_name=u'Заголовок')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from=lambda i: u'%s-%s' % (i.title, i.published_at))
  subtitle = models.CharField(max_length=1024, verbose_name=u'Подзаголовок', blank=True)
  status = models.CharField(choices=STATUS, default=STATUS.draft, max_length=20, verbose_name=u'Статус')
  type = models.CharField(choices=TYPES, default=TYPES.text, max_length=20, verbose_name=u'Тип содержимого')
  published_at = models.DateTimeField(verbose_name=u'Время публикации', default=lambda: datetime.now())
  selected_at = models.DateTimeField(verbose_name=u'Дата выбора в топ', null=True, blank=True)
  cover_source = models.ImageField(upload_to='articles/covers', verbose_name=u'Обложка', blank=True)
  announce = models.TextField(verbose_name=u'Анонс (краткое содержание)')
  content = models.TextField(verbose_name=u'Текст', default='')
  authors = models.ManyToManyField(UserModel, verbose_name=u'Авторы', blank=True, related_name='articles', limit_choices_to={
    'level__gte': UserModel.USER_LEVELS.trusted
  })
  topics = models.ManyToManyField(Topic, verbose_name=u'Темы', blank=True, related_name='articles')

  # TODO: remove after migration
  gazetted = models.BooleanField(default=False)

  comments_count = models.PositiveIntegerField(editable=False, default=0)
  views_count = models.PositiveIntegerField(editable=False, default=0)

  # not mapped stuff
  objects = models.Manager()
  published = PublishedManager()
  events = EventsManager()
  news = NewsManager()
  wod = WODManager()
  blogs = BlogsManager()

  main_cover_for_wod = ImageSpec([ResizeToFit(428, 180, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_sidebar = ImageSpec([ResizeToFit(200, 150, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_eventbox = ImageSpec([ResizeToFit(200, 200, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_main_selection = ImageSpec([ResizeToFit(140, 128, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  inside_article_cover = ImageSpec([ResizeToFit(345, 345, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  inside_wod_article_cover =  ImageSpec([ResizeToFit(900, 399, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_wodlist = ImageSpec([ResizeToFit(390, 170, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_video = ImageSpec([ResizeToFit(246, 184, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')

  class Meta:
    ordering = ['-published_at']
    verbose_name = u'Статья'
    verbose_name_plural = u'Статьи'

  @staticmethod
  def get_most_commented():
    #end = datetime.now()
    #start = end - oneday * 30
    return Article.published.prefetch_related('authors').select_related().all()[0:5]

  @models.permalink
  def get_absolute_url(self):
    return ('content.views.article', (), {'slug': self.slug})

  @property
  def issue(self):
    # TODO: CACHE!!!
    try:
      issue = RubricInIssue.objects.get(
        issue__published_at = self.published_at,
        rubric = self.rubric
      ).issue
    except RubricInIssue.DoesNotExist:
      issue = None
    return issue

  def render_content(self):
    if self.type == Article.TYPES.text:
      return self.content
    else:
      tpl = """<iframe type="text/html" width="640" height="480" src="%s" frameborder="0" allowfullscreen></iframe>"""
      pc = urlparse(self.content)
      if pc.netloc.endswith("youtube.com"):
        source = "http://youtube.com/embed/%s?html5=1" % parse_qs(pc.query).get('v')[0].strip()
      else:
        source = self.content
      return tpl % source      

  def __unicode__(self):
    return u'%s' % self.title


class ExpertComment(models.Model):
  expert = models.ForeignKey(settings.AUTH_USER_MODEL, verbose_name=u'Эксперт', related_name='expert_comments')
  article = models.ForeignKey(Article, verbose_name=u'Статья', related_name='expert_comments')
  comment = models.TextField(verbose_name=u'Текст')
  position = models.PositiveIntegerField(verbose_name=u'Позиция')

  class Meta:
    ordering = ['-position']
    verbose_name = u'Комментарий эксперта'
    verbose_name_plural = u'Комментарии экспертов'

  def __unicode__(self):
    return u'%s' % self.expert


class News(Article):
  class Meta:
    proxy = True
    verbose_name = u'Новость'
    verbose_name_plural = u'Новости'


class Wod(Article):
  class Meta:
    proxy = True
    verbose_name = u'Слово Дня'
    verbose_name_plural = u'Слова Дня'

  objects = WODManager()


class DailyQuote(models.Model):
  class Meta:
    ordering = ['-day']
    verbose_name = u'Цитата дня'
    verbose_name_plural = u'Цитаты дня'

  quote = models.TextField(verbose_name=u'Цитата')
  source = models.ForeignKey(Article, verbose_name=u'Источник цитаты')
  day = models.DateField(verbose_name=u'День', unique=True, default=lambda: datetime.now())
