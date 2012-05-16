#-*- coding: utf-8 -*-
from django.db import models
from django.contrib.contenttypes.models import ContentType

from autoslug import AutoSlugField

class TitledSlugEntry(models.Model):
    class Meta:
	abstract = True
    title = models.CharField(max_length=1024, verbose_name=u'Название')
    slug  = AutoSlugField(max_length=1024, unique=True, editable=False, populate_from='title')

class WithDenormalizedStats(models.Model):
    class Meta:
	abstract = True
    rating         = models.BigIntegerField(default=0, verbose_name=u'Рейтинг', editable=False)
    views_count    = models.BigIntegerField(default=0, verbose_name=u'Кол-во просмотров', editable=False)
    comments_count = models.BigIntegerField(default=0, verbose_name=u'Кол-во комментариев', editable=False)

def get_content_type(obj):
    # TODO: cache whole content-type mapping
    return ContentType.objects.get_for_model(obj.__class__)