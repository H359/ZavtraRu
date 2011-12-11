#-*- coding: utf-8 -*-
from datetime import datetime

from django.conf import settings
from django.db import models
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes import generic
from django.contrib.auth.models import User

from treebeard.mp_tree import MP_Node

class Comment(MP_Node):
    class Meta:
	ordering = ['path']
	permissions = (
	    ('moderate', u'Может модерировать комментарии'),
	)
	verbose_name=u'Комментарий'
	verbose_name_plural=u'Комментарии'
    content_type   = models.ForeignKey(ContentType)
    object_id      = models.PositiveIntegerField()
    content_object = generic.GenericForeignKey('content_type', 'object_id')
    author         = models.ForeignKey(User, verbose_name=u'Автор')
    comment        = models.TextField(verbose_name=u'Текст комментария')
    enabled        = models.BooleanField(default=True)
    created_at     = models.DateTimeField(editable=False, default=lambda: datetime.now())
    rating         = models.IntegerField(default=0)

    node_order_by = ['created_at']

    def short_comment(self):
	return ('-'*self.depth) + self.comment[0:50] + '...'
    short_comment.short_description = u'Краткое содержание'

    def get_author(self):
	author = self.author
	username = u' '.join([author.first_name.strip(), author.last_name.strip()])
	if username != u' ':
	    return username
	return author.username

import signals