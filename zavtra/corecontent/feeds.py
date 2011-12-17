#-*- coding: utf-8 -*-
from datetime import datetime
from django.contrib.syndication.views import Feed
from django.shortcuts import get_object_or_404, Http404
from django.conf import settings
from django.utils import feedgenerator

from models import ContentItem, Rubric, Tag, FeaturedItems

class LatestContentFeed(Feed):
    feed_type = feedgenerator.Rss201rev2Feed
    ttl = 600
    title =u'Газета Завтра - новые материалы'
    link = u'/'
    description = u'Сводная лента обновлений'
    description_template = 'corecontent/feeds/feed.item.html'
    title_template = 'corecontent/feeds/feed.title.html'

    def items(self):
	now = datetime.now().date()
	return ContentItem.batched.batch_select('authors').filter(enabled=True, pub_date__lte = now)[0:10]

    def item_author_name(self, item):
	return u', '.join(map(lambda w: u'%s %s' % (w.first_name, w.last_name), item.authors_all))

class RubricContentFeed(Feed):
    feed_type = feedgenerator.Rss201rev2Feed
    description_template = 'corecontent/feeds/feed.item.html'
    title_template = 'corecontent/feeds/feed.title.html'
    ttl = 600

    def description(self, obj):
	return u'Лента обновлений рубрики %s' % obj.title

    def title(self, obj):
	return u'Газета Завтра - рубрика %s' % obj.title

    def items(self, obj):
	now = datetime.now().date()
	return ContentItem.batched.batch_select('authors').filter(rubric=obj, enabled=True, pub_date__lte = now)[0:10]

    def link(self, obj):
	return obj.get_absolute_url()

    def item_author_name(self, item):
	return u', '.join(map(lambda w: u'%s %s' % (w.first_name, w.last_name), item.authors_all))

    def get_object(self, request, slug):
	return get_object_or_404(Rubric, slug=slug)

class TagContentFeed(Feed):
    feed_type = feedgenerator.Rss201rev2Feed
    description_template = 'corecontent/feeds/feed.item.html'
    title_template = 'corecontent/feeds/feed.title.html'
    ttl = 600

    def description(self, obj):
	return u'Лента обновлений тега %s' % obj.name

    def title(self, obj):
	return u'Газета Завтра - всё по тегу %s' % obj.name

    def items(self, obj):
	now = datetime.now().date()
	return ContentItem.batched.batch_select('authors').filter(tags=obj, enabled=True, pub_date__lte = now)[0:10]

    def link(self, obj):
	return obj.get_absolute_url()

    def item_author_name(self, item):
	return u', '.join(map(lambda w: u'%s %s' % (w.first_name, w.last_name), item.authors_all))

    def get_object(self, request, slug):
	return get_object_or_404(Tag, slug=slug)

class FeaturedItemsContentFeed(Feed):
    feed_type = feedgenerator.Rss201rev2Feed
    description_template = 'corecontent/feeds/feed.item.html'
    title_template = 'corecontent/feeds/feed.title.html'
    ttl = 600

    def description(self, obj):
	return u'Лента обновлений горячей темы %s' % obj.title

    def title(self, obj):
	return u'Газета Завтра - горячая тема %s' % obj.title

    def items(self, obj):
	now = datetime.now().date()
	return ContentItem.batched.batch_select('authors').filter(tags__in=obj.tags_all, enabled=True, pub_date__lte = now)[0:10]

    def link(self, obj):
	return obj.get_absolute_url()

    def item_author_name(self, item):
	return u', '.join(map(lambda w: u'%s %s' % (w.first_name, w.last_name), item.authors_all))

    def get_object(self, request, slug):
	try:
	    return FeaturedItems.batched.batch_select('tags').get(slug=slug)
	except FeaturedItems.DoesNotExist:
	    raise Http404