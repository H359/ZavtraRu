#-*- coding: utf-8 -*-
from django.db import models
from django.contrib.auth.models import User
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes import generic

from model_utils import TitledSlugEntry

class Poll(TitledSlugEntry):
    class Meta:
	abstract = False
    content_type   = models.ForeignKey(ContentType, null=True)
    object_id      = models.IntegerField()
    content_object = generic.GenericForeignKey('content_type', 'object_id')
    created_at     = models.DateTimeField(verbose_name=u'Время создания', auto_now_add=True)
    allow_multiple = models.BooleanField(default=False)

    user_has_voted = False

class Option(models.Model):
    poll     = models.ForeignKey(Poll, related_name='options')
    position = models.PositiveIntegerField()
    option   = models.CharField(max_length=255)

class Answer(models.Model):
    poll       = models.ForeignKey(Poll)
    user       = models.ForeignKey(User)
    created_at = models.DateTimeField(verbose_name=u'Время голосования', auto_now_add=True)
    chosen     = models.ForeignKey(Option)

def get_polls_for_item(item, user):
    polls = Poll.objects.prefetch_related('options').filter(object_id=item.pk, content_type=ContentType.objects.get_for_model(item.__class__))
    check = Answer.objects.filter(poll__in=polls, user=user).values_list('poll_id', flat=True)
    for c in check:
	poll = filter(lambda w: w.id == c, polls)[0]
	poll.user_has_voted = True
    return polls