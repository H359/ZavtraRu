#-*- coding: utf-8 -*-
from django.conf import settings
from django.db import models
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes import generic
from django.contrib.auth.models import User

from mptt.models import MPTTModel

AUTH_BACKENDS = ('zavtra',) +settings.SOCIAL_AUTH_ENABLED_BACKENDS

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
    provider       = models.IntegerField(choices=enumerate(AUTH_BACKENDS), editable=False, default=0)
    
    def save(self, *args, **kwargs):
	"""
        try:
            assoc = self.author.social_auth.all()[0]
            print assoc.extra_data
            #self.provider = assoc.provider
        except IndexError:
            pass
        """
        super(Comment, self).save(*args, **kwargs)
        self.content_object.update_comments_count()

    def get_author(self):
	author = self.author
	username = u' '.join([author.first_name.strip(), author.last_name.strip()])
	if username != u' ':
	    return username
	return author.username