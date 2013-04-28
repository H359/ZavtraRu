# -*- coding: utf-8 -*-
from django.db import models

from autoslug import AutoSlugField


class TitledSluggedModel(models.Model):
  title = models.CharField(max_length=20, verbose_name=u'Заголовок')
  slug = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from='title')

  class Meta:
    abstract = True
