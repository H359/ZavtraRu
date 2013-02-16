# -*- coding: utf-8 -*-
from datetime import datetime
from pytils.translit import slugify
from urlparse import urlparse, parse_qs

from django.db import models
from django.db.models import Count
from django.conf import settings
from django.utils.html import strip_tags

from model_utils import Choices
from djorm_pgfulltext.models import SearchManager
from djorm_pgfulltext.fields import VectorField
from autoslug import AutoSlugField
from imagekit.models import ImageSpec
from imagekit.processors.resize import ResizeToFit, ResizeToFill
from imagekit.processors.crop import Crop, Anchor

from siteuser.models import User as UserModel

from managers import PublishedManager, EventsManager, WODManager,\
                     NewsManager, ColumnsManager
from zavtra.utils import cached, oneday, OpenGraphMixin


class Rubric(models.Model):
  title = models.CharField(max_length=1024, verbose_name=u'Название')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from='title')
  in_rubricator = models.BooleanField(default=True, verbose_name=u'Включать в рубрикатор')

  def __unicode__(self):
    return u'%s' % self.title

  class Meta:
    verbose_name = u'Рубрика'
    verbose_name_plural = u'Рубрики'

  @models.permalink
  def get_absolute_url(self):
    return ('content.views.rubric', (), {'slug': self.slug})

  @staticmethod
  def fetch_rubric(slug):
    return cached(lambda: Rubric.objects.get(slug=slug), 'rubric:%s' % slug)

  @staticmethod
  def get_gazette_rubrics():
    # TODO: optimize this
    rii = RubricInIssue.objects.filter(rubric__in_rubricator=True).distinct('rubric').\
          values_list('rubric', flat=True).order_by('rubric', 'position')
    return Rubric.objects.filter(pk__in = rii)


class Issue(models.Model):
  absolute_number = models.PositiveIntegerField(verbose_name=u'Номер (абсолютный)')
  relative_number = models.PositiveIntegerField(verbose_name=u'Номер (относительный)')
  published_at = models.DateField(verbose_name=u'Дата выхода')
  illustration = models.ImageField(upload_to='zhivotov', verbose_name=u'Иллюстрация Животова')

  objects = models.Manager()
  published = PublishedManager()

  #gazette_box = ImageSpec([ResizeToFit(278, 121, True, 0xFFFFFF)], image_field='illustration', format='JPEG')
  gazette_box = ImageSpec([ResizeToFill(278, 121)], image_field='illustration', format='JPEG')
  zeitung_box = ImageSpec([ResizeToFill(870, 385)], image_field='illustration', format='JPEG')
  archive_box = ImageSpec([ResizeToFill(750, 300)], image_field='illustration', format='JPEG')
  inside_article_cover = ImageSpec([ResizeToFit(345, 345, True, 0xFFFFFF)], image_field='illustration', format='JPEG')

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
    ).prefetch_related('authors').select_related().defer('content')
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

  def __unicode__(self):
    return u'%s' % self.rubric

  class Meta:
    ordering = ['position']
    verbose_name = u'Рубрика в выпуске'
    verbose_name_plural = u'Рубрика в выпуске'


class Topic(models.Model):
  title = models.CharField(max_length=1024, verbose_name=u'Название')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from='title')
  position = models.PositiveIntegerField(verbose_name=u'Позиция', default=0)
  on_top = models.BooleanField(verbose_name=u'Выводить в главном меню', default=False)

  def __unicode__(self):
    return u'%s' % self.title

  class Meta:
    ordering = ['position']
    verbose_name = u'Тема'
    verbose_name_plural = u'Темы'

  @models.permalink
  def get_absolute_url(self):
    return ('content.views.topic', (), {'slug': self.slug})

  @staticmethod
  def autocomplete_search_fields():
    return ("id__iexact", "title__icontains",)


class Article(OpenGraphMixin, models.Model):
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
  columns = ColumnsManager()

  main_cover_for_wod = ImageSpec([ResizeToFill(428, 281)], image_field='cover_source', format='JPEG')
  cover_for_sidebar = ImageSpec([ResizeToFill(200, 150)], image_field='cover_source', format='JPEG')
  cover_for_eventbox = ImageSpec([ResizeToFill(200, 200)], image_field='cover_source', format='JPEG')
  cover_for_main_selection = ImageSpec([ResizeToFill(140, 128)], image_field='cover_source', format='JPEG')
  inside_article_cover = ImageSpec([ResizeToFill(345, 345)], image_field='cover_source', format='JPEG')
  inside_wod_article_cover =  ImageSpec([ResizeToFill(900, 399)], image_field='cover_source', format='JPEG')
  cover_for_wodlist = ImageSpec([ResizeToFill(390, 170)], image_field='cover_source', format='JPEG')
  cover_for_topbar = ImageSpec([ResizeToFill(150, 105)], image_field='cover_source', format='JPEG')
  cover_for_video = ImageSpec([Crop(640, 360, anchor=Anchor.BOTTOM), ResizeToFill(246, 184)], image_field='cover_source', format='JPEG')

  def __unicode__(self):
    return u'%s' % self.title

  class Meta:
    ordering = ['-published_at']
    verbose_name = u'Статья'
    verbose_name_plural = u'Статьи'

  @models.permalink
  def get_absolute_url(self):
    return ('content.views.article', (), {'slug': self.slug})

  @property
  def issue(self):
    if not hasattr(self, '__issue_cache'):
      try:
        issue = RubricInIssue.objects.get(
          issue__published_at = self.published_at,
          rubric = self.rubric
        ).issue
      except RubricInIssue.DoesNotExist:
        issue = None
      self.__issue_cache = issue
    return self.__issue_cache

  @property
  def open_graph_data(self):
    yield ('og:type', 'article')
    yield ('og:title', self.title)
    yield ('og:description', strip_tags(self.announce))
    for author in self.authors.all():
      yield ('og:author', 'http://%s%s' % (settings.APP_DOMAIN, author.get_absolute_url()))
    for topic in self.topics.all():
      yield ('og:tag', topic.title)
    yield ('article:published_time', self.published_at.strftime('%Y-%m-%d'))
    if self.cover_source != "":
      yield ('og:image', self.inside_article_cover.url)
    elif self.is_peredovitsa and self.issue and self.issue.illustration != "":
      yield ('og:image', self.issue.inside_article_cover.url)

  @property
  def is_peredovitsa(self):
    return self.rubric.id == Rubric.fetch_rubric('peredovitsa').id

  def render_content(self, width=640, height=480):
    if self.type == Article.TYPES.text:
      return self.content
    else:
      tpl = """<iframe type="text/html" width="%d" height="%d" src="%s" frameborder="0" allowfullscreen></iframe>"""
      pc = urlparse(self.content)
      if pc.netloc.endswith("youtube.com"):
        source = "http://youtube.com/embed/%s?html5=1" % parse_qs(pc.query).get('v')[0].strip()
      else:
        source = self.content
      return tpl % (width, height, source)

  @staticmethod
  def get_most_commented():
    #end = datetime.now()
    #start = end - oneday * 30
    return Article.published.prefetch_related('authors').select_related().all()[0:5]


class ExpertComment(models.Model):
  expert = models.ForeignKey(settings.AUTH_USER_MODEL, verbose_name=u'Эксперт', related_name='expert_comments')
  article = models.ForeignKey(Article, verbose_name=u'Статья', related_name='expert_comments')
  comment = models.TextField(verbose_name=u'Текст')
  position = models.PositiveIntegerField(verbose_name=u'Позиция')

  def __unicode__(self):
    return u'%s' % self.expert

  class Meta:
    ordering = ['-position']
    verbose_name = u'Комментарий эксперта'
    verbose_name_plural = u'Комментарии экспертов'


class DailyQuote(models.Model):
  quote = models.TextField(verbose_name=u'Цитата')
  source = models.ForeignKey(Article, verbose_name=u'Источник цитаты')
  day = models.DateField(verbose_name=u'День', unique=True, default=lambda: datetime.now())

  class Meta:
    ordering = ['-day']
    verbose_name = u'Цитата дня'
    verbose_name_plural = u'Цитаты дня'
