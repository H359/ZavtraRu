#-*- coding: utf-8 -*-
import urllib2
import urlparse

from django.db.models.signals import post_save, pre_delete
from django.core.cache import cache
from django.dispatch import receiver

from models import Comment

def update_comments_count(sender, **kwargs):
    instance = kwargs['instance']
    target = instance.content_object
    if target and hasattr(target, 'update_comments_count'):
	target.update_comments_count()

receiver(post_save, sender=Comment, dispatch_uid='zavtra.comments.signals')(update_comments_count)
receiver(pre_delete, sender=Comment, dispatch_uid='zavtra.comments.signals')(update_comments_count)