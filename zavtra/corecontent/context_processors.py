#-*- coding: utf-8 -*-
from datetime import datetime, timedelta

from django.utils.html import strip_tags

from corecontent.models import Rubric, FeaturedItems, DailyQuote, ContentItem

from utils import cached

def news_stripper(item):
    item.description = strip_tags(item.description)
    return item

def common_pieces(request):
    if request.path.startswith('/admin'):
	return {}
    now = datetime.now()
    if request.is_ajax():
	base_template = 'base_ajax.html'
    else:
	base_template = 'base.html'
    top_rubrics  = cached(lambda: list(Rubric.objects.filter(on_top=True).exclude(title=u'Новости')), 'top_rubrics', duration=600)
    featured = cached(lambda: list(FeaturedItems.objects.filter(is_active=True)), 'featured', duration=60)
    try:
	quote = cached(lambda: DailyQuote.objects.filter(day=now.date()), 'quote', duration=600)[0]
    except IndexError:
	quote = None
    news = cached(lambda: map(news_stripper, ContentItem.objects.filter(enabled=True, pub_date__lte=now, rubric__title=u'Новости')[0:6]), 'news2', duration=120)
    current_items = cached(
	lambda: ContentItem.batched.batch_select('authors').select_related().exclude(rubric = 1).filter(enabled=True, published=False)[0:12],
	'red_string',
	duration=120
    )
    return {
	'news': news,
	'base_template': base_template,
        'top_rubrics': top_rubrics,
        'featured': featured,
        'quote': quote,
        'current_items': current_items
    }
