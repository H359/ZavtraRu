#-*- coding: utf-8 -*-
from datetime import datetime, timedelta

from django.utils.html import strip_tags

from corecontent.models import Rubric, FeaturedItems, DailyQuote, ContentItem
from django.db.models import Count
from minipoll.models import Poll

from utils import cached

def news_stripper(item):
    item.description = strip_tags(item.description)
    return item

def get_quote(date):
    try:
	quote = DailyQuote.objects.select_related().filter(day=date)[0]
    except IndexError:
	quote = None
    return quote

def get_latest_poll(date):
    return Poll.calculate(Poll.objects.filter(status=1).annotate(total_votes=Count('vote')).latest('creation_date'))

def common_pieces(request):
    now      = datetime.now()
    date     = now.date()
    rubrics  = cached(lambda: list(Rubric.objects.all()), 'rubrics', duration=60*60*24)
    featured = cached(lambda: list(FeaturedItems.objects.filter(is_active=True)), 'featured', duration=60*60*24)
    quote    = cached(lambda: get_quote(date), 'quote', duration=60*60*4)
    news     = cached(lambda: map(news_stripper, ContentItem.objects.filter(enabled=True, pub_date__lte=now, rubric__title=u'Новости')[0:10]), 'news2', duration=60*60*24)
    poll     = cached(lambda: get_latest_poll(date), 'latest_poll_object', duration=5*60)
    current_items = cached(
	lambda: ContentItem.batched.batch_select('authors').select_related().exclude(rubric = 1).filter(enabled=True, published=False)[0:12],
	'red_string',
	duration=60*60*24
    )
    return {
	'news':          news,
        'rubrics':       rubrics,
        'featured':      featured,
        'quote':         quote,
        'current_items': current_items,
        'current_poll':  poll,
        'banners_pool': []
    }
