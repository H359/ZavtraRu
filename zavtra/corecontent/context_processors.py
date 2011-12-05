#-*- coding: utf-8 -*-
from datetime import datetime

from corecontent.models import Rubric, FeaturedItems, DailyQuote

from utils import cached

def common_pieces(request):
    rubrics  = cached(lambda: Rubric.objects.exclude(title=u'Новости'), 'rubrics')
    featured = cached(lambda: FeaturedItems.objects.filter(is_active=True), 'featured')
    try:
	quote = cached(lambda: DailyQuote.objects.filter(day=datetime.now().date()), 'quote')[0]
    except IndexError:
	quote = None
    return {
        'rubrics': rubrics,
        'featured': featured,
        'quote': quote
    }
