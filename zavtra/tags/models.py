#-*- coding: utf-8 -*-
from django.db import models
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes import generic
from django.conf import settings

from autoslug import AutoSlugField

class Tag(models.Model):
    class Meta:
        verbose_name=u'Тег'
        verbose_name_plural=u'Теги'
    title = models.CharField(max_length=250, verbose_name=u'Название', unique=True)
    kind  = models.CharField(max_length=250, verbose_name=u'Тип')
    slug  = AutoSlugField(populate_from=lambda instance: instance.title)

    @staticmethod
    def register(model):
        setattr(model, 'tags', generic.GenericRelation(Tag))

class TaggedItem(models.Model):
    tag = models.ForeignKey(Tag)
    content_type = models.ForeignKey(ContentType)
    object_id = models.PositiveIntegerField()
    content_object = generic.GenericForeignKey('content_type', 'object_id')

try:
    for module, models in settings.TAGGED_MODELS:
        klasses = __import__(module, globals(), locals(), models, -1)
        for model in models:
            klass = getattr(klasses, model)
            Tag.register(klass)
except AttributeError, e:
    pass
