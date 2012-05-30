#-*- coding: utf-8 -*-
from datetime import datetime
from django.contrib.syndication.views import Feed
from django.shortcuts import get_object_or_404, Http404
from django.conf import settings
from django.utils import feedgenerator
from django.utils.html import strip_tags
from email import utils
import time

from yafeed import YandexNewsRss
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

class ExclusiveNewsFeed(Feed):
    feed_type = YandexNewsRss
    ttl = 600
    title =u'Газета Завтра'
    link = u'http://zavtra.ru'
    description = u'Еженедельная федеральная газета'

    def items(self):
        now = datetime.now().date()
        return ContentItem.batched.batch_select('authors').filter(enabled=True, exclusive=True, pub_date__lte = now)[0:10]

    def item_title(self, item):
        title = strip_tags(item.title)
        if title.endswith('.'): title = title[0:-1]
        return title

    def item_description(self, item):
        return strip_tags(item.description)

    def item_extra_kwargs(self, item):
        return {
            'fulltext': strip_tags(item.content),
            'pubDate': utils.formatdate(time.mktime(item.pub_date.timetuple()), True)
        }

    def item_author_name(self, item):
        return u', '.join(map(lambda w: u'%s %s' % (w.first_name, w.last_name), item.authors_all))

class UnpublishedContentFeed(Feed):
    feed_type = feedgenerator.Rss201rev2Feed
    ttl = 600
    title =u'Газета Завтра - чего нет в гаезете'
    link = u'/'
    description = u'Сводная лента обновлений, не вошедших в бумажный выпуск'
    description_template = 'corecontent/feeds/feed.item.html'
    title_template = 'corecontent/feeds/feed.title.html'

    def items(self):
        now = datetime.now().date()
        return ContentItem.batched.batch_select('authors').filter(enabled=True, published=False, pub_date__lte = now)[0:10]

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
        now = datetime.now()
        return ContentItem.batched.batch_select('authors').filter(rubric=obj, enabled=True, pub_date__lte = now)[0:10]

    def link(self, obj):
        return obj.get_absolute_url()

    def item_author_name(self, item):
        return u', '.join(map(lambda w: u'%s %s' % (w.first_name, w.last_name), item.authors_all))

    def item_pubdate(self, item):
        return item.pub_date

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