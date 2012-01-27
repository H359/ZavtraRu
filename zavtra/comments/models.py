#-*- coding: utf-8 -*-
from datetime import datetime

from django.conf import settings
from django.db import models
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes import generic
from django.contrib.auth.models import User

from mail.models import EmailTemplate
from treebeard.mp_tree import MP_Node

class Comment(models.Model):
    class Meta:
	ordering = ('path',)
	permissions = (
	    ('moderate', u'Может модерировать комментарии'),
	)
	verbose_name=u'Комментарий'
	verbose_name_plural=u'Комментарии'
    parent         = models.ForeignKey('self', null=True, blank=True, verbose_name=u'Родительский комментарий', related_name='children')
    path          = models.CharField(max_length=1024)
    content_type   = models.ForeignKey(ContentType)
    object_id      = models.PositiveIntegerField()
    content_object = generic.GenericForeignKey('content_type', 'object_id')
    author         = models.ForeignKey(User, verbose_name=u'Автор', related_name='comments')
    comment        = models.TextField(verbose_name=u'Текст комментария', max_length=8192, help_text=u'Максимум 8192 символа. Помните &mdash; это комментарий, а не статья.')
    enabled        = models.BooleanField(default=True)
    created_at     = models.DateTimeField(editable=False, default=lambda: datetime.now())
    rating         = models.IntegerField(default=0)
    ip             = models.IPAddressField(verbose_name=u'IP отправителя', blank=True)

    def save(self, *args, **kwargs):
	# TODO: bailout on present path?
	lookup = {'content_type': self.content_type, 'object_id': self.object_id, 'parent': self.parent}
	if self.pk is not None:
	    lookup['pk__lt'] = self.pk
	sibling_number = Comment.objects.filter(**lookup).count()
	if self.parent is not None:
	    prev_path = self.parent.path
	else:
	    prev_path = ""
	chunk = ['0' for _ in range(0,self.step_size)]
	fill, rem = divmod(sibling_number, len(self.alphabet))
	for x in range(0, fill):
	    chunk[self.step_size - x - 1] = self.alphabet[-1]
	chunk[self.step_size - fill - 1] = self.alphabet[rem]
	self.path = prev_path + ''.join(chunk)
	super(Comment, self).save(*args, **kwargs)
	if self.id and not self.enabled:
	    return
	if self.parent_id is not None:
	    parent = self.parent
	    if parent.author_id != self.author_id and len(parent.author.email) > 0:
		tpl = EmailTemplate.get('comments.reply_added')
		tpl.send(receivers=[parent.author.email], data={'original': self.parent, 'reply': self})

    def short_comment(self):
	return ('-'*self.depth) + self.comment[0:50] + '...'
    short_comment.short_description = u'Краткое содержание'

    def depth(self):
	return (len(self.path) / self.step_size) - 1

    def depth_truncated(self):
	return min([4, self.depth()])

    def get_author(self):
	author = self.author
	if author.first_name or author.last_name:
	    return author.get_full_name()
	else:
	    return author.username

    @models.permalink
    def get_absolute_url(self):
	return ('resolve_content_object', (), {'content_type_id': self.content_type_id, 'id': self.object_id})

    @staticmethod
    def calculate_paths():
	for comment in Comment.objects.select_related().all():
	    comment.save()

    @staticmethod
    def paths2parents():
	for comment in Comment.objects.all():
	    Comment.objects.filter(pk=comment.pk).update(parent = comment.get_parent())

Comment.alphabet = map(chr, range(48,58) + range(65,91) + range(97,123))
Comment.step_size = 6

import signals