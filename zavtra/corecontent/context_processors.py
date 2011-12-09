#-*- coding: utf-8 -*-
from datetime import datetime

from corecontent.models import Rubric, FeaturedItems, DailyQuote

from utils import cached

def common_pieces(request):
    if request.is_ajax():
	base_template = 'base_ajax.html'
    else:
	base_template = 'base.html'
    top_rubrics  = cached(lambda: list(Rubric.objects.filter(on_top=True).exclude(title=u'Новости')), 'top_rubrics')
    featured = cached(lambda: list(FeaturedItems.objects.filter(is_active=True)), 'featured')
    try:
	quote = cached(lambda: list(DailyQuote.objects.filter(day=datetime.now().date())), 'quote')[0]
    except IndexError:
	quote = None
    return {
	'base_template': base_template,
        'top_rubrics': top_rubrics,
        'featured': featured,
        'quote': quote
    }
