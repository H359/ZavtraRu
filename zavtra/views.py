#-*- coding: utf-8 -*-
import math
from datetime import datetime, timedelta
from itertools import groupby

from django.http import HttpResponseRedirect
from django.contrib.auth import logout as auth_logout, login as user_login, authenticate
from django.contrib.auth.models import User
from django.shortcuts import redirect, get_object_or_404
from django.views.decorators.csrf import csrf_exempt
from django.template.loader import render_to_string
from django.utils.functional import lazy
from django.contrib.auth.context_processors import PermWrapper

from annoying.decorators import render_to, ajax_request

from corecontent.models import ContentItem, ZhivotovIllustration
from comments.models import Comment

from utils import cached

oneday = timedelta(days=1)

@render_to('home.html')
def home(request):
    no_cache = False
    now = datetime.now().date()
    wstart = now - oneday*(now.weekday()+5)
    if now.weekday() > 2:
	wstart += 7*oneday
    wend = wstart + 7*oneday
    print wstart, wend
    if request.user.is_authenticated() and request.user.is_staff and request.GET.get('next_number'):
	wstart += 7*oneday
	wend += 7*oneday
	no_cache = True
    num = 49 + math.floor( (now - datetime(year=2011,month=12,day=7).date()).days / 7.0 )
    def get_illustration():
	p = ZhivotovIllustration.objects.filter(pub_date__range = (wstart, wend))
	try:
	    return p[0]
	except IndexError:
	    return None
    def get_content():
        qs = ContentItem.batched.batch_select('authors').select_related('rubric').filter(
	    enabled=True, pub_date__gte = wstart, pub_date__lt = wend, rubric__on_main=True
	)
	newsletter = {}
	for item in list(qs):
	    newsletter.setdefault(item.rubric_id, {'rubric': None, 'items': []})
	    if newsletter[item.rubric_id]['rubric'] is None:
		newsletter[item.rubric_id]['rubric'] = item.rubric
	    newsletter[item.rubric_id]['items'].append(item)
	return sorted(newsletter.values(), key=lambda p: p['rubric'].position)
    if not no_cache:
	newsletter = cached(
	    get_content,
	    'newsletter',
	    duration=600
	)
    else:
	newsletter = get_content()
    illustration = cached(
	get_illustration,
	'illustration',
	duration=6000
    )
    return {
	'issue_info': { 'date': wstart, 'num': num },
	'newsletter': newsletter,
	'illustration': illustration,
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

@render_to('live.html')
def live(request):
    return {'stream': Comment.objects.filter(enabled=True).select_related().order_by('-id')[0:10]}

@csrf_exempt
@ajax_request
def live_update(request):
    last_seen = request.POST.get('last_seen')
    qs = Comment.objects.filter(enabled=True).select_related().filter(pk__gt = last_seen).order_by('-id')[0:50]
    if request.user.is_authenticated() and request.user.is_staff:
	perms = lazy(lambda: PermWrapper(request.user), PermWrapper)()
    else:
	perms = None
    rendered = [render_to_string('comments/item.html', {'comment': x, 'perms': perms, 'request': request, 'stream': True}) for x in qs]
    return {'stream': rendered}