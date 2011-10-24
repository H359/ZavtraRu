#-*- coding: utf-8 -*-
from django.db import models
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes import generic
from django.contrib.auth.models import User

from mptt.models import MPTTModel

class Comment(MPTTModel):
    class Meta:
        ordering = ('tree_id', 'lft')
    parent         = models.ForeignKey('self', null=True, blank=True, related_name='children')
    content_type   = models.ForeignKey(ContentType)
    object_id      = models.PositiveIntegerField()
    content_object = generic.GenericForeignKey('content_type', 'object_id')
    author         = models.ForeignKey(User)
    comment        = models.TextField(verbose_name=u'Текст')
    enabled        = models.BooleanField(default=True)
    created_at     = models.DateTimeField(auto_now_add=True)
