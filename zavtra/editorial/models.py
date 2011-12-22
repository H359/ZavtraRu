#-*- coding: utf-8 -*-
from datetime import datetime

from django.db import models
from django.contrib.auth.models import User

from treebeard.mp_tree import MP_Node

class Thread(MP_Node):
    class Meta:
	verbose_name=u'Задача'
	ordering=['path']
    title      = models.CharField(max_length=250, verbose_name=u'Название', blank=True, null=True)
    text       = models.TextField(blank=True, null=True, verbose_name='Текст')
    author     = models.ForeignKey(User, verbose_name=u'Автор')
    created_at = models.DateField(default=lambda: datetime.now(), editable=False, verbose_name=u'Дата создания')

    node_order_by = ['created_at']
    
    @models.permalink
    def get_absolute_url(self):
	return ('editorial.view.thread', (), {'id': self.id})