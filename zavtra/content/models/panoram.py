#-*- coding: utf-8 -*-
from django.db import models
from imagekit.models import ImageSpecField as ImageSpec
from imagekit.processors.resize import ResizeToFill
from imagekit.processors.crop import Crop, Anchor

from base import TitledSluggedModel
from topic import Topic


class Panoram(TitledSluggedModel):
  topics   = models.ManyToManyField(Topic, verbose_name=u'Темы')
  on_main  = models.BooleanField(default=False, verbose_name=u'На главной')
  position = models.PositiveIntegerField()
  image    = models.ImageField(upload_to='panoram', verbose_name=u'Фон')

  main_page_image = ImageSpec([ResizeToFill(200, 150)], source='image', format='JPEG')


  class Meta:
    verbose_name = u'Панорама'
    verbose_name_plural = u'Панорамы'
    ordering = ('position',)
    app_label = 'content'

  def __unicode__(self):
    return self.title

  @models.permalink
  def get_absolute_url(self):
    return ('content.view.panoram', (), {'slug': self.slug})