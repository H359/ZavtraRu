# -*- coding: utf-8 -*-
from django.db import models
from django.contrib.auth.models import User
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes import generic

from mptt.models import MPTTModel
from markitup.fields import MarkupField

from model_utils import TitledSlugEntry, WithDenormalizedStats

class Rubric(TitledSlugEntry, MPTTModel):
    parent = models.ForeignKey('self', blank=True, null=True, related_name='children')

class Article(TitledSlugEntry, WithDenormalizedStats):
    sub_title = models.CharField(max_length=1024, blank=True)
    authors   = models.ManyToManyField(User)
    pub_date  = models.DateTimeField()
    published = models.BooleanField()
    enabled   = models.BooleanField()
    announce  = models.TextField()
    rubric    = models.ForeignKey(Rubric)
    exclusive = models.BooleanField()

    # compiled from parts
    content   = models.TextField(editable=False)

class ArticlePart(models.Model):
    article        = models.ForeignKey(Article)
    position       = models.PositiveIntegerField()
    content_type   = models.ForeignKey(ContentType, null=True)
    object_id      = models.IntegerField()
    content_object = generic.GenericForeignKey('content_type', 'object_id')

class MarkdownText(models.Model):
    text = models.TextField()

class YoutubeVideo(models.Model):
    url = models.URLField()