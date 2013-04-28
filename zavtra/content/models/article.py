# -*- coding: utf-8 -*-
from urlparse import urlparse, parse_qs

from django.db import models
from django.conf import settings
from django.utils.html import strip_tags

from model_utils import Choices
from imagekit.models import ImageSpec
from imagekit.processors.resize import ResizeToFill
from imagekit.processors.crop import Crop, Anchor
from djorm_pgfulltext.models import SearchManager
from djorm_pgfulltext.fields import VectorField
from djorm_expressions.models import ExpressionManager

from base import TitledSluggedModel
from managers import *
from issue import Issue, RubricInIssue
from rubric import Rubric
from topic import Topic
from zavtra.utils import cached, oneday, OpenGraphMixin
from siteuser.models import User as UserModel


class Article(OpenGraphMixin, TitledSluggedModel):
  STATUS = Choices(('draft', u'Черновик'), ('ready', u'Готово к публикации'))
  TYPES = Choices(('text', u'Текст'), ('video', u'Видео'))

  rubric = models.ForeignKey(Rubric, verbose_name=u'Рубрика', related_name='articles')
  subtitle = models.CharField(max_length=50, verbose_name=u'Подзаголовок', blank=True)
  status = models.CharField(choices=STATUS, default=STATUS.draft, max_length=20, verbose_name=u'Статус')
  type = models.CharField(choices=TYPES, default=TYPES.text, max_length=20, verbose_name=u'Тип содержимого')
  published_at = models.DateTimeField(verbose_name=u'Время публикации', default=lambda: datetime.now())
  selected_at = models.DateTimeField(verbose_name=u'Дата выбора в топ', null=True, blank=True)
  cover_source = models.ImageField(upload_to='articles/covers', verbose_name=u'Обложка', blank=True)
  announce = models.TextField(verbose_name=u'Анонс (краткое содержание)')
  content = models.TextField(verbose_name=u'Текст', default='')
  authors = models.ManyToManyField(settings.AUTH_USER_MODEL, verbose_name=u'Авторы', blank=True, related_name='articles', limit_choices_to={
    'level__gte': UserModel.USER_LEVELS.trusted
  })
  rating = models.IntegerField(editable=False, default=0)
  topics = models.ManyToManyField(Topic, verbose_name=u'Темы', blank=True, related_name='articles')
  show_icon = models.BooleanField(verbose_name=u'Показывать иконку внутри статьи?', default=True)

  # TODO: remove after migration
  gazetted = models.BooleanField(default=False, editable=False)

  # denorm
  comments_count = models.PositiveIntegerField(editable=False, default=0)
  views_count = models.PositiveIntegerField(editable=False, default=0)
  _issue = models.IntegerField(editable=False, default=-1)

  search_index = VectorField()

  # not mapped stuff
  objects = ExpressionManager()
  published = PublishedManager()
  events = EventsManager()
  news = NewsManager()
  common_news = BaseNewsManager()
  wod = WODManager()
  columns = ColumnsManager()
  editorial = EditorialManager()

  searcher = SearchManager(
    fields = (('title', 'A'), ('subtitle', 'B'), ('content', 'C')),
    config = 'pg_catalog.russian',
    search_field = 'search_index',
    auto_update_search_field = True
  )

  # thumbs
  main_cover_for_wod = ImageSpec([ResizeToFill(428, 321)], image_field='cover_source', format='JPEG')
  cover_for_sidebar = ImageSpec([ResizeToFill(200, 150)], image_field='cover_source', format='JPEG')
  cover_for_eventbox = ImageSpec([ResizeToFill(200, 200)], image_field='cover_source', format='JPEG')
  #cover_for_main_selection = ImageSpec([ResizeToFill(140, 128)], image_field='cover_source', format='JPEG')
  cover_for_main_selection = ImageSpec([ResizeToFill(203, 152)], image_field='cover_source')
  inside_article_cover = ImageSpec([ResizeToFill(345, 259)], image_field='cover_source', format='JPEG')
  inside_wod_article_cover =  ImageSpec([ResizeToFill(900, 675)], image_field='cover_source', format='JPEG')
  cover_for_wodlist = ImageSpec([ResizeToFill(390, 292)], image_field='cover_source', format='JPEG')
  cover_for_topbar = ImageSpec([ResizeToFill(150, 79)], image_field='cover_source', format='JPEG')
  cover_for_video = ImageSpec([Crop(640, 360, anchor=Anchor.BOTTOM), ResizeToFill(246, 184)], image_field='cover_source', format='JPEG')

  def __unicode__(self):
    return u'%s' % self.title
  
  def save(self, *args, **kwargs):
    if self._issue > 0:
      self._issue = -1
    super(Article, self).save(*args, **kwargs)

  def update_search_field(self, *args, **kwargs):
    self._fts_manager.update_search_field(pk=self.pk)

  class Meta:
    ordering = ['-published_at']
    verbose_name = u'Статья'
    verbose_name_plural = u'Статьи'
    app_label = 'content'
    abstract = False

  @models.permalink
  def get_absolute_url(self):
    return ('content.view.article', (), {'slug': self.slug})

  @property
  def external_image(self):
    pc = urlparse(self.content)
    if pc.netloc.endswith("youtube.com"):
      vid = parse_qs(pc.query).get('v')[0].strip()
      return 'http://img.youtube.com/vi/%s/0.jpg' % vid
    else:
      return ''

  @property
  def issue(self):
    if not hasattr(self, '__issue_cache'):
      if self._issue > 0:
        issue = Issue.published.get(id=self._issue)
      # early bailout for non-gazette articles
      elif self._issue == 0 or not self.rubric.from_zeitung:
        issue = None
      else:
        try:
          issue = RubricInIssue.objects.select_related().get(
            issue__published_at = self.published_at,
            rubric = self.rubric
          ).issue
          Article.objects.filter(pk=self.pk).update(_issue = issue.id)
        except RubricInIssue.DoesNotExist:
          issue = None
      if issue is None:
        self._issue = 0
      setattr(self, '__issue_cache', issue)
    return getattr(self, '__issue_cache')

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

  @property
  def source(self):
    if not hasattr(self, '__source_cached'):
      pc = urlparse(self.content)
      if pc.netloc.endswith("youtube.com"):
        source = 'youtube:%s' % parse_qs(pc.query).get('v')[0].strip()
      else:
        source ='dentv:%s' % pc.path
      setattr(self, '__source_cached', source)
    return getattr(self, '__source_cached')

  def render_content(self, width=640, height=480):
    # TODO: cache rendered content?
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
    def most_commented():
      return Article.published.\
             exclude(rubric = Rubric.fetch_rubric('novosti')).\
             filter(published_at__gte = datetime.now() - oneday * 30).\
             order_by('-comments_count', '-published_at').\
             prefetch_related('authors').\
             select_related().\
             defer('content')[0:5]
    return cached(most_commented, 'content:most-commented', 60)

  @staticmethod
  def autocomplete_search_fields():
    return ("id__iexact", "title__icontains",)

class ArticleVote(models.Model):
  article = models.ForeignKey(Article, related_name='votes')
  user = models.ForeignKey(settings.AUTH_USER_MODEL)
  vote = models.SmallIntegerField(default=0)

  class Meta:
    unique_together = ('article', 'user')
    app_label = 'content'

  def save(self, *args, **kwargs):
    super(ArticleVote, self).save(*args, **kwargs)
    #print ArticleVote.objects.filter(article=self.article).aggregate(rating=Sum('vote'))
    Article.published.filter(id=self.article_id).update(
      **ArticleVote.objects.filter(article=self.article).aggregate(rating=models.Sum('vote'))
    )
