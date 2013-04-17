# -*- coding: utf-8 -*-
from django.db import models

from imagekit.models import ImageSpec
from imagekit.processors.resize import ResizeToFill
from imagekit.processors.crop import Crop

from managers import PublishedManager
from rubric import Rubric


class Issue(models.Model):
  absolute_number = models.PositiveIntegerField(verbose_name=u'Номер (абсолютный)')
  relative_number = models.PositiveIntegerField(verbose_name=u'Номер (относительный)')
  published_at = models.DateField(verbose_name=u'Дата выхода')
  illustration = models.ImageField(upload_to='zhivotov', verbose_name=u'Иллюстрация Животова')
  illustration_text = models.CharField(
    verbose_name=u'Подпись к иллюстрации',
    help_text=u'Если пустое, то будет взят заголовок передовицы.',
    max_length=128,
    blank=True,
    null=True,
    default=None
  )

  objects = models.Manager()
  published = PublishedManager()

  gazette_box = ImageSpec([ResizeToFill(278, 121)], image_field='illustration')
  zeitung_box = ImageSpec([ResizeToFill(870, 385)], image_field='illustration')
  archive_box = ImageSpec([ResizeToFill(750, 300)], image_field='illustration')
  inside_article_cover = ImageSpec([ResizeToFill(345, 345)], image_field='illustration')
  cover_for_sidebar = ImageSpec([ResizeToFill(200, 150)], image_field='illustration')

  class Meta:
    ordering = ['-published_at']
    verbose_name = u'Выпуск'
    verbose_name_plural = u'Выпуск'
    app_label = 'content'

  @property
  def gazette(self):
    # TODO: cache?
    from article import Article
    issue_rubrics = list(self.issue_rubrics.select_related().all())
    articles = Article.objects.filter(
      published_at__year=self.published_at.year,
      published_at__month=self.published_at.month,
      published_at__day=self.published_at.day,
      rubric__in=[x.rubric for x in issue_rubrics]
    ).prefetch_related('authors').select_related().defer('content')
    rubric_positions = {r.rubric_id: r.position for r in issue_rubrics}
    return sorted(articles, key=lambda a: rubric_positions[a.rubric_id])

  @property
  def gazette_selected(self):
    from article import Article
    issue_rubrics = list(self.issue_rubrics.select_related().all())
    articles = Article.objects.filter(
      models.Q(selected_at__isnull=False) | models.Q(rubric=Rubric.fetch_rubric('peredovitsa')),
      published_at__year=self.published_at.year,
      published_at__month=self.published_at.month,
      published_at__day=self.published_at.day,
      rubric__in=[x.rubric for x in issue_rubrics]
    ).prefetch_related('authors').select_related().defer('content').\
    order_by('selected_at')
    def keyer(a):
      if a.is_peredovitsa:
        return 1
      else:
        return a.selected_at.toordinal()
    return sorted(list(articles), key=keyer)

  @models.permalink
  def get_absolute_url(self):
    kwargs = {
      'year': self.published_at.year,
      'issue': self.relative_number
    }
    return ('content.view.zeitung', (), kwargs)

  def get_pdf_link(self):
    return "http://zavtra.ru/media/content/pdfs/%dPDF.zip" % self.relative_number


class RubricInIssue(models.Model):
  issue = models.ForeignKey(Issue, verbose_name=u'Выпуск', related_name='issue_rubrics')
  rubric = models.ForeignKey(Rubric, verbose_name=u'Рубрика', related_name='issue_rubrics')
  position = models.PositiveIntegerField(verbose_name=u'Позиция')

  def __unicode__(self):
    return u'%s' % self.rubric

  class Meta:
    ordering = ['position']
    verbose_name = u'Рубрика в выпуске'
    verbose_name_plural = u'Рубрика в выпуске'
    app_label = 'content'
