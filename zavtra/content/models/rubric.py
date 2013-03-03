# -*- coding: utf-8 -*-
from django.db import models

from base import TitledSluggedModel

from zavtra.utils import cached


class Rubric(TitledSluggedModel):
  in_rubricator = models.BooleanField(default=True, verbose_name=u'Включать в рубрикатор')

  def __unicode__(self):
    return u'%s' % self.title

  class Meta:
    verbose_name = u'Рубрика'
    verbose_name_plural = u'Рубрики'
    app_label = 'content'

  @property
  def from_zeitung(self):
    return self.id in [x.id for x in Rubric.get_gazette_rubrics()]

  @models.permalink
  def get_absolute_url(self):
    return ('content.view.rubric', (), {'slug': self.slug})

  @staticmethod
  def fetch_rubric(slug):
    return cached(lambda: Rubric.objects.get(slug=slug), 'rubrics:rubric-%s' % slug, 3600)

  @staticmethod
  def get_gazette_rubrics():
    def inner():
      from issue import RubricInIssue
      rii = RubricInIssue.objects.distinct('rubric').\
            values_list('rubric', flat=True).order_by('rubric', 'position')
      return list(Rubric.objects.filter(pk__in = rii))
    return cached(inner, 'rubrics:gazette', 3600)