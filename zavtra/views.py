#-*- coding: utf-8 -*-
from datetime import datetime, timedelta
import time
from itertools import groupby

from django.http import HttpResponse, HttpResponseRedirect
from django.db.models import F
from django.contrib.auth import logout as auth_logout, login as user_login, authenticate
from django.contrib.auth.models import User
from django.contrib.auth.decorators import login_required
from django.contrib.auth.context_processors import PermWrapper
from django.contrib.contenttypes.models import ContentType
from django.shortcuts import redirect, get_object_or_404
from django.views.generic import ListView
from django.template.loader import render_to_string
from django.utils.functional import lazy
from django.conf import settings

from annoying.decorators import render_to, ajax_request
from diggpaginator import DiggPaginator

from corecontent.models import ContentItem, ZhivotovIllustration
from comments.models import Comment
from voting.models import Vote

from utils import cached, group_list

oneday = timedelta(days=1)

@render_to('home.html')
def home(request):
    no_cache = False
    now = datetime.now().date()
    wstart = now - oneday*(now.weekday()+5)
    if now.weekday() >= 2:
	wstart += 7*oneday
    wend = wstart + 7*oneday
    if request.user.is_authenticated() and request.user.is_staff and request.GET.get('next_number'):
	wstart += 7*oneday
	wend += 7*oneday
	no_cache = True
    num = 1 + (wstart - datetime(year=wstart.year,day=1,month=1).date()).days / 7
    def get_illustration():
	try:
	    zh = ZhivotovIllustration.objects.latest('pub_date')
	except ZhivotovIllustration.DoesNotExist:
	    zh = None
	return zh
    def get_content():
        qs = ContentItem.batched.batch_select('authors').select_related('rubric').filter(
	    enabled=True, rubric__on_main=True, pub_date__gte = wstart, pub_date__lt = wend, published=True
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
	    duration=60*60*4
	)
    else:
	newsletter = get_content()
    def get_latest_rubric(rubric, kind='video'):
	try:
	    return ContentItem.objects.filter(kind=kind, enabled=True, rubric=rubric).latest('pub_date')
	except ContentItem.DoesNotExist:
	    return None
    illustration = cached(
	get_illustration,
	'illustration',
	duration=6000
    )
    zavtra_tv = cached(
	lambda: ContentItem.objects.filter(kind='video', enabled=True, rubric=19 if settings.DEBUG else 44)[0:1],
	'zavtra-tv2',
	duration=60*60*4
    )
    special_project = cached(
	lambda: get_latest_rubric(47),
	'special-project',
	duration=60*60*4
    )
    return {
	'issue_info': { 'date': wstart, 'num': num },
	'newsletter': newsletter,
	'illustration': illustration,
	'zavtra_tv': zavtra_tv,
	'special_project': special_project
    }

def resolver(request, content_type_id, id):
    ctype = get_object_or_404(ContentType, id=content_type_id)
    obj = get_object_or_404(ctype.model_class(), id=id)
    return redirect(obj.get_absolute_url())

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

def logout(request):
    auth_logout(request)
    return HttpResponseRedirect('/')

def logged_in(request):
    raise Exception, "OLOLO!"

@render_to('live.html')
def live(request):
    return {}

@ajax_request
def live_update(request):
    qty = int(request.GET.get('qty', 20))
    start = request.GET.get('start')
    comments = Comment.objects.filter(enabled=True).order_by('-id')
    if start is not None and start != 'null':
	comments = comments.filter(created_at__gte = datetime.fromtimestamp(int(start)))
    return {
	'stream': map(lambda c: render_to_string('comments/item.html', {'stream': True, 'comment':c}), reversed(comments[0:qty])),
    }

login_required
def vote(request):
    if request.method == 'POST':
	vote = {'up': 1, 'down': -1}[request.POST.get('vote')]
	djct = int(request.POST.get('djct'))
	djoi = int(request.POST.get('djoi'))
	ct = get_object_or_404(ContentType, pk=djct)
	try:
	    v = Vote.objects.get(user=request.user, content_type=ct, object_id=djoi)
	except Vote.DoesNotExist:
	    v = Vote(user=request.user, content_type=ct, object_id=djoi)
	model = ct.model_class()
	obj = get_object_or_404(model, pk=djoi)
	if v.vote != vote:
	    v.vote = vote
	    v.save()
	if request.is_ajax():
	    return HttpResponse()
	else:
	    return redirect(obj.get_absolute_url())