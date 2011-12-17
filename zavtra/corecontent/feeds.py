#-*- coding: utf-8 -*-
from django.contrib.syndication.views import Feed
from django.conf import settings

from models import ContentItem

class LatestContentFeed(Feed):
    title =u'Новые материалы газеты Завтра'
    link = u'/'
    description = u'Сводная лента обновлений'
    description_template = 'corecontent/feeds/feed.item.html'
    title_template = 'corecontent/feeds/feed.title.html'

    def items(self):
	return ContentItem.objects.all()[0:5]

class RubricContentFeed(Feed):
    pass

class TagContentFeed(Feed):
    pass

class FeaturedItemsContentFeed(Feed):
    pass