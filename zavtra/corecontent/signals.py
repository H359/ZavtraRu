#-*- coding: utf-8 -*-
import urllib2
import urlparse

from django.db.models.signals import post_save, pre_delete
from django.core.cache import cache
from django.dispatch import receiver

from social_auth.signals import socialauth_registered

from voting.models import Vote

from models import ContentItem, Article, Video, Image, Rubric, FeaturedItems, NewsItem

"""
def update_cache(sender, **kwargs):
    if kwargs['instance'].rubric is not None:
	if kwargs['instance'].rubric.title == u'Новости':
	    cache.delete('news')
	else:
	    cache.delete('rubric-%d-items' % kwargs['instance'].rubric_id)
    else:
	cache.delete('blogs-stream')

for klass in [Article, Video, NewsItem, Image]:
    receiver(post_save, sender=klass, dispatch_uid='corecontent_updatecache')(update_cache)
    receiver(pre_delete, sender=klass, dispatch_uid='corecontent_updatecaches')(update_cache)

@receiver(post_save, sender=Rubric, dispatch_uid='rubrics_updatecache')
def update_rubrics(sender, **kwargs):
    cache.delete('rubrics')

@receiver(post_save, sender=FeaturedItems, dispatch_uid='featureditems_updatecache')
def update_featured(sender, **kwargs):
    cache.delete('featured')
"""
"""
@receiver(socialauth_registered, sender=None, dispatch_uid='zavtra.corecontent.signals')
def auth_registered(sender, user, response, details, **kwargs):
    #sender = TwitterBackend | FacebookBackend ...
    print sender
    #twitter -- response.profile_image_url_url, response.description, 
    #facebook -- response.username, response.first_name, response.last_name, response.link, response.id
    print response, details
"""

"""
@receiver(socialauth_registered, sender=None, dispatch_uid='zavtra.corecontent.signals')
def auth_registered(sender, user, response, details, **kwargs):
    #sender = TwitterBackend | ...
    print sender
    #twitter -- avatar : profile_image_url_https, description
    
    print response, details

from voting.models import Vote

from models import ContentItem, Rubric, contentitem_ctype_id

@receiver(post_save, sender=Rubric, dispatch_uid='zavtra.corecontent.signals')
def update_rubrics(sender, **kwargs):
    kwargs['instance'].reset_content_items()

@receiver(post_save, sender=ContentItem, dispatch_uid='zavtra.corecontent.signals')
def update_cache(sender, **kwargs):
    if kwargs['instance'].rubric is not None:
        key = 'rubric-%d-content-items' % kwargs['instance'].rubric_id
        cache.set(key, ContentItem.objects.batch_select('authors').filter(enabled=True).filter(rubric=kwargs['instance'].rubric)[0:3], 60*60*24)
"""
@receiver(post_save, sender=Vote, dispatch_uid='zavtra.corecontent.signals')
def update_rating(sender, **kwargs):
    if kwargs['instance'].content_type_id == contentitem_ctype_id:
        #kwargs['instance'].object.rating = Vote.objects.get_score(kwargs['instance'])['score']
        kwargs['instance'].object.recalculate_rating()