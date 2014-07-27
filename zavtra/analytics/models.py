#-*- coding: utf-8 -*-
from django.db import models
from django.conf import settings
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes import generic


class Hit(models.Model):
    datetime       = models.DateTimeField(null=False, auto_now_add=True)
    user           = models.ForeignKey(settings.AUTH_USER_MODEL, null=True)
    content_type   = models.ForeignKey(ContentType)
    object_id      = models.PositiveIntegerField()
    content_object = generic.GenericForeignKey('content_type', 'object_id')
