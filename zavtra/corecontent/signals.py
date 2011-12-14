#-*- coding: utf-8 -*-
import urllib2
import urlparse
import tweepy

from django.db.models.signals import post_save, pre_delete
from django.core.cache import cache
from django.conf import settings
from django.dispatch import receiver

from social_auth.signals import socialauth_registered

from voting.models import Vote

from models import ContentItem, Article, Video, Image, Rubric, FeaturedItems, NewsItem

def maketh_twitter():
    auth = tweepy.OAuthHandler(settings.TWEEPY_CK, settings.TWEEPY_CS)
    auth.set_access_token(settings.TWEEPY_AT, settings.TWEEPY_ATS)
    api = tweepy.API(auth)
    return api

def maketh_status(instr):
    curlen = 0
    res = []
    parts = filter(lambda w: len(w) > 0, instr.strip().split(' '))
    for part in parts:
	if len(part) + curlen + len(res) > 120:
	    break
	else:
	    res.append(part)
	    curlen += len(part)
    return ' '.join(res)

# TODO: defer this
@receiver(post_save, sender=Article, dispatch_uid='corecontent.signals.article.tweet')
def tweet_article(sender, **kwargs):
    if kwargs['created'] and kwargs['instance'].enabled and not settings.DEBUG:
        api = maketh_twitter()
        status = '%s http://zavtra.ru%s' % (maketh_status(kwargs['instance'].title), kwargs['instance'].get_absolute_url())
        api.update_status(status)

@receiver(post_save, sender=Vote, dispatch_uid='zavtra.corecontent.signals')
def update_rating(sender, **kwargs):
    if kwargs['instance'].content_type_id == contentitem_ctype_id:
        #kwargs['instance'].object.rating = Vote.objects.get_score(kwargs['instance'])['score']
        kwargs['instance'].object.recalculate_rating()