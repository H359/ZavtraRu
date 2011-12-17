#-*- coding: utf-8 -*-
from datetime import datetime, timedelta

from corecontent.models import Rubric, FeaturedItems, DailyQuote, ContentItem

from utils import cached

def common_pieces(request):
    now = datetime.now().date()
    if request.is_ajax():
	base_template = 'base_ajax.html'
    else:
	base_template = 'base.html'
    top_rubrics  = cached(lambda: list(Rubric.objects.filter(on_top=True).exclude(title=u'Новости')), 'top_rubrics', duration=600)
    featured = cached(lambda: list(FeaturedItems.objects.filter(is_active=True)), 'featured', duration=60)
    try:
	quote = cached(lambda: list(DailyQuote.objects.filter(day=now)), 'quote', duration=600)[0]
    except IndexError:
	quote = None
    news = cached(lambda: ContentItem.objects.filter(enabled=True, pub_date__lte=now, rubric__title=u'Новости')[0:5], 'news', duration=30)
    return {
	'news': news,
	'base_template': base_template,
        'top_rubrics': top_rubrics,
        'featured': featured,
        'quote': quote
    }
