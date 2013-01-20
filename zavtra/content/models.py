# -*- coding: utf-8 -*-
from datetime import datetime
from pytils.translit import slugify
from urlparse import urlparse, parse_qs

from django.db import models
from django.db.models import Count
from django.conf import settings

#from mptt.models import MPTTModel, TreeForeignKey
from model_utils import Choices
from djorm_pgfulltext.models import SearchManager
from djorm_pgfulltext.fields import VectorField
from autoslug import AutoSlugField
from imagekit.models import ImageSpec
from imagekit.processors.resize import ResizeToFit

from managers import PublishedManager, EventsManager, WODManager, NewsManager
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

  #@models.permalink
  def get_absolute_url(self):
    return None


class Issue(models.Model):
  absolute_number = models.PositiveIntegerField(verbose_name=u'Номер (абсолютный)')
  relative_number = models.PositiveIntegerField(verbose_name=u'Номер (относительный)')
  published_at = models.DateField(verbose_name=u'Дата выхода')
  illustration = models.ImageField(upload_to='zhivotov', verbose_name=u'Иллюстрация Животова')

  published = PublishedManager()

  class Meta:
    ordering = ['-published_at']
    verbose_name = u'Выпуск'
    verbose_name_plural = u'Выпуск'

  @property
  def gazette(self):
    # TODO: cache?
    issue_rubrics = list(self.issue_rubrics.all())
    articles = list(Article.objects.filter(published_at=self.published_at).\
               filter(rubric__in = [x.rubric for x in issue_rubrics]).\
               select_related())
    rubric_positions = {r.rubric_id: r.position for r in issue_rubrics}
    return sorted(articles, key=lambda a: rubric_positions[a.rubric_id])

  #@models.permalink
  def get_absolute_url(self):
    return None


class RubricInIssue(models.Model):
  issue = models.ForeignKey(Issue, verbose_name=u'Выпуск', related_name='issue_rubrics')
  rubric = models.ForeignKey(Rubric, verbose_name=u'Рубрика')
  position = models.PositiveIntegerField(verbose_name=u'Позиция')

  class Meta:
    ordering = ['-position']
    verbose_name = u'Рубрика в выпуске'
    verbose_name_plural = u'Рубрика в выпуске'


class Topic(models.Model):
  title = models.CharField(max_length=1024, verbose_name=u'Название')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from='title')
  position = models.PositiveIntegerField(verbose_name=u'Позиция', default=0)
  on_top = models.BooleanField(verbose_name=u'Выводить в главном меню', default=False)

  class Meta:
    ordering = ['-position']
    verbose_name = u'Тема'
    verbose_name_plural = u'Тема'

  #@models.permalink
  def get_absolute_url(self):
    return None


class Article(models.Model):
  STATUS = Choices(('draft', u'Черновик'), ('ready', u'Готово к публикации'))
  TYPES = Choices(('text', u'Текст'), ('video', u'Видео'))

  rubric = models.ForeignKey(Rubric, verbose_name=u'Категория', related_name='articles')
  title = models.CharField(max_length=1024, verbose_name=u'Заголовок')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from=lambda i: u'%s-%s' % (i.title, i.published_at))
  subtitle = models.CharField(max_length=1024, verbose_name=u'Подзаголовок', blank=True)
  status = models.CharField(choices=STATUS, default=STATUS.draft, max_length=20)
  type = models.CharField(choices=TYPES, default=TYPES.text, max_length=20)
  published_at = models.DateTimeField(verbose_name=u'Время публикации')
  selected_at = models.DateTimeField(verbose_name=u'Дата выбора в топ', null=True, blank=True)
  cover_source = models.ImageField(upload_to='articles/covers', verbose_name=u'Обложка', blank=True)
  announce = models.TextField(verbose_name=u'Анонс (краткое содержание)')
  content = models.TextField(verbose_name=u'Текст', default='')
  authors = models.ManyToManyField(settings.AUTH_USER_MODEL, verbose_name=u'Авторы', blank=True, related_name='articles')
  topics = models.ManyToManyField(Topic, verbose_name=u'Темы', blank=True, related_name='articles')

  comments_count = models.PositiveIntegerField(editable=False, default=0)
  views_count = models.PositiveIntegerField(editable=False, default=0)

  # not mapped stuff
  objects = models.Manager()
  published = PublishedManager()
  events = EventsManager()
  news = NewsManager()
  wod = WODManager()

  main_cover_for_wod = ImageSpec([ResizeToFit(428, 180, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')

  class Meta:
    ordering = ['-published_at']
    verbose_name = u'Статья'
    verbose_name_plural = u'Статьи'

  @models.permalink
  def get_absolute_url(self):
    return ('content.views.article', (), {'slug': self.slug})


class ExpertComment(models.Model):
  expert = models.ForeignKey(settings.AUTH_USER_MODEL, verbose_name=u'Эксперт', related_name='expert_comments')
  article = models.ForeignKey(Article, verbose_name=u'Статья', related_name='expert_comments')
  comment = models.TextField(verbose_name=u'Текст')
  position = models.PositiveIntegerField(verbose_name=u'Позиция')

  class Meta:
    ordering = ['-position']
    verbose_name = u'Комментарий эксперта'
    verbose_name_plural = u'Комментарии экспертов'

"""
class ZhivotovIllustration(models.Model):
  image = models.ImageField(upload_to='zhivotov', verbose_name=u'Изображение')
  published_at = models.DateTimeField(verbose_name=u'Время публикации')
  #title = models.CharField(max_length=200, verbose_name=u'Название'')

  archive_box = ImageSpec([ResizeToFit(870, 385, True)], image_field='image', format='JPEG')
  gazette_box = ImageSpec([ResizeToFit(278, 121, True)], image_field='image', format='JPEG')
  slider_box = ImageSpec([ResizeToFit(150, 105, True)], image_field='image', format='JPEG')

  class Meta:
    ordering = ['-published_at']
    verbose_name = u'Иллюстрация Животова'
    verbose_name_plural = u'Иллюстрации Животова'

  def __unicode__(self):
    return u'%s' % self.published_at

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
    if self.events_rubric:
      return ('content.views.events_now',) #, (), {})
    else:
      return ('content.views.rubric', (), {'slug': self.slug})

  def __unicode__(self):
    return u'%s %s' % ('-'*self.level, self.title)

  @property
  def zeitung_rubric(self):
    zeitung = Rubric.fetch_rubric('zeitung')
    return self.pk in map(lambda w: w.pk, zeitung)

  @property
  def wod_rubric(self):
    wod = Rubric.fetch_rubric('wod')
    return self.pk in map(lambda w: w.pk, wod)

  @property
  def events_rubric(self):
    events = Rubric.fetch_rubric('events')
    return self.pk in map(lambda w: w.pk, events)

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
  authors = models.ManyToManyField(settings.AUTH_USER_MODEL, verbose_name=u'Авторы', blank=True, related_name='articles')
  announce = models.TextField(verbose_name=u'Анонс (краткое содержание)')
  cover_source = models.ImageField(upload_to='articles/covers', verbose_name=u'Обложка')
  content = models.TextField(verbose_name=u'Текст', default='')
  topics = models.ManyToManyField(Topic, verbose_name=u'Темы', blank=True, related_name='articles')

  # managers
  objects = PublishedArticlesManager()
  zeitung = ZeitungManager()
  blogs = BlogsManager()
  news = NewsManager()
  events = EventsManager()
  columns = ColumnsManager()
  editorial = EditorialManager()
  wod = WODManager()

  # old `objects` manager
  everything = models.Manager()

  # covers
  cover_for_main = ImageSpec([ResizeToFit(200, 150, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_wod = ImageSpec([ResizeToFit(428, 180, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_eventbox = ImageSpec([ResizeToFit(200, 200, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_list = ImageSpec([ResizeToFit(140, 128, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_video = ImageSpec([ResizeToFit(246, 184, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_article = ImageSpec([ResizeToFit(345, 345, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_wodarticle = ImageSpec([ResizeToFit(900, 399, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')
  cover_for_wodlist = ImageSpec([ResizeToFit(390, 170, True, 0xFFFFFF)], image_field='cover_source', format='JPEG')

  class Meta:
    ordering = ['-published_at']
    verbose_name = u'Статья'
    verbose_name_plural = u'Статьи'

  @models.permalink
  def get_absolute_url(self):
    return ('content.views.article', (), {'slug': self.slug})

  def __unicode__(self):
    return u'%s' % self.title

  def render_content(self):
    if self.type == Article.TYPES.text:
      return self.content
    else:
      tpl = <iframe type=\"text/html\" width=\"640\" height=\"480\" src=\"%s\" frameborder=\"0\" allowfullscreen></iframe>"
      pc = urlparse(self.content)
      if pc.netloc.endswith("youtube.com"):
        source = "http://youtube.com/embed/%s?html5=1" % parse_qs(pc.query).get('v')[0].strip()
      else:
        source = self.content
      return tpl % source      

  @property
  def from_zeitung(self):
    return self.rubric.zeitung_rubric

  @property
  def from_wod(self):
    return self.rubric.wod_rubric

  @property
  def from_events(self):
    return self.rubric.events_rubric

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
  def get_most_commented():
    return Article.objects.annotate(comments_count = Count('comments')).order_by('-comments_count')

  @staticmethod
  def get_current_issue_date_range():
    now = datetime.now().date()
    if settings.DEBUG:
      now -= 7*8 * oneday
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


class ExpertComment(models.Model):
  expert = models.ForeignKey(settings.AUTH_USER_MODEL, verbose_name=u'Эксперт', related_name='expert_comments')
  article = models.ForeignKey(Article, verbose_name=u'Статья', related_name='expert_comments')
  comment = models.TextField(verbose_name=u'Текст')
  position = models.PositiveIntegerField(verbose_name=u'Позиция')

  class Meta:
    ordering = ['-position']
    verbose_name = u'Комментарий эксперта'
    verbose_name_plural = u'Комментарии экспертов'


class DailyQuote(models.Model):
  class Meta:
    ordering = ['-day']
    verbose_name = u'Цитата дня'
    verbose_name_plural = u'Цитаты дня'

  quote = models.TextField(verbose_name=u'Цитата')
  source = models.ForeignKey(Article, verbose_name=u'Источник цитаты')
  day = models.DateField(verbose_name=u'День', unique=True, default=lambda: datetime.now())
"""