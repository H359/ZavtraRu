#-*- coding: utf-8 -*-
from datetime import datetime, timedelta

from corecontent.models import Rubric, FeaturedItems, DailyQuote

from utils import cached

def common_pieces(request):
    def get_issue_info():
	now = datetime.now()
	oneday = timedelta(days=1)
	num = 49 + (datetime.now().date() - datetime(year=2011,month=12,day=7).date()).days / 7
	date = now - oneday*(now.weekday()+5)
	if now.weekday() > 2:
	    date += 7*oneday
	return {'num': num, 'date': date}
    issue_number = cached(
	get_issue_info,
	'issue_number_info',
	duration=600
    )
    if request.is_ajax():
	base_template = 'base_ajax.html'
    else:
	base_template = 'base.html'
    top_rubrics  = cached(lambda: list(Rubric.objects.filter(on_top=True).exclude(title=u'Новости')), 'top_rubrics', duration=600)
    featured = cached(lambda: list(FeaturedItems.objects.filter(is_active=True)), 'featured', duration=60)
    try:
	quote = cached(lambda: list(DailyQuote.objects.filter(day=datetime.now().date())), 'quote', duration=600)[0]
    except IndexError:
	quote = None
    return {
	'issue_number': issue_number,
	'base_template': base_template,
        'top_rubrics': top_rubrics,
        'featured': featured,
        'quote': quote
    }
