#-*- coding: utf-8 -*-
from datetime import datetime, timedelta
from itertools import groupby

from django.http import HttpResponseRedirect
from django.contrib.auth import logout as auth_logout, login as user_login, authenticate
from django.contrib.auth.models import User
from django.shortcuts import redirect, get_object_or_404

from annoying.decorators import render_to

from corecontent.models import ContentItem, ZhivotovIllustration

from utils import cached

oneday = timedelta(days=1)

@render_to('home.html')
def home(request):
    now = datetime.now().date()
    wstart = now - oneday*(now.weekday() - 2)
    wend = wstart + 7*oneday
    def get_illustration():
	p = ZhivotovIllustration.objects.filter(pub_date__range = (wstart, wend))
	try:
	    return p[0]
	except IndexError:
	    return None
    def get_content():
        qs = ContentItem.batched.batch_select('authors').select_related('rubric').filter(
	    enabled=True, pub_date__range = (wstart, wend), rubric__on_main=True
	)
	newsletter = {}
	for item in list(qs):
	    newsletter.setdefault(item.rubric_id, {'rubric': None, 'items': []})
	    if newsletter[item.rubric_id]['rubric'] is None:
		newsletter[item.rubric_id]['rubric'] = item.rubric
	    newsletter[item.rubric_id]['items'].append(item)
	return sorted(newsletter.values(), key=lambda p: p['rubric'].position)
    newsletter = cached(
	get_content,
	'newsletter',
	duration=600
    )
    """
    blogs = cached(
	lambda: ContentItem.batched.batch_select('authors').filter(enabled=True, rubric=None)[0:6],
	'blogs',
	duration = 60
	#duration = (wend - now).seconds
    )
    """
    illustration = cached(
	get_illustration,
	'illustration',
	duration=600
    )
    news = cached(lambda: ContentItem.objects.filter(rubric__title=u'Новости')[0:5], 'news', duration=30)
    return {
	'newsletter': newsletter,
	'illustration': illustration,
	'news': news
    }

@render_to('user.html')
def user(request, username):
    user = get_object_or_404(User, username=username)
    return {'user': user}

@render_to('login.html')
def login(request):
    if request.user.is_authenticated():
	return redirect('/')
    if request.method == 'POST':
	username = request.POST.get('username')
	password = request.POST.get('password')
	user = authenticate(username=username, password=password)
	if user is not None:
	    print user, user.is_active
	    if user.is_active:
		user_login(request, user)
		return redirect('/')
	    else:
		return {'login_fail': True}
	else:
	    return {'login_fail': True}
    return {}

@render_to('archive.html')
def archive(request):
    issues = sorted(Issue.objects.order_by('date'), key=lambda issue: issue.date.year, reverse=True)
    return {
        'in_archive': True,
        'issues': [(k, list(g)) for k, g in groupby(issues, lambda w: w.date.year)]
    }

def logout(request):
    auth_logout(request)
    return HttpResponseRedirect('/')

def logged_in(request):
    raise Exception, "OLOLO!"