#-*- coding: utf-8 -*-
from datetime import datetime, timedelta
import time
from itertools import groupby

from django.http import HttpResponse, HttpResponseRedirect
from django.db.models import F, Count
from django.contrib.auth import logout as auth_logout, login as user_login, authenticate
from django.contrib.auth.models import User
from django.contrib.auth.decorators import login_required
from django.contrib.auth.context_processors import PermWrapper
from django.contrib.contenttypes.models import ContentType
from django.shortcuts import redirect, get_object_or_404
from django.views.generic import ListView
#from django.template.loader import render_to_string
from django.utils.functional import lazy
from django.conf import settings

from annoying.decorators import ajax_request
from diggpaginator import DiggPaginator

from corecontent.models import ContentItem, ZhivotovIllustration, contentitem_ctype_id
from comments.models import Comment
from voting.models import Vote

from utils import MakoViewMixin, templateLookup, render_to, cached, group_list

oneday = timedelta(days=1)

class SearchView(MakoViewMixin, ListView):
    paginate_by = 15
    paginator_class = DiggPaginator
    template_name = 'search/search.html'
    MATCH_MODES = {
        'phrase': 'SPH_MATCH_PHRASE',
        'any': 'SPH_MATCH_ANY',
        'all': 'SPH_MATCH_ALL'
    }
    INV_MATCH_MODES = dict((v,k) for k, v in MATCH_MODES.iteritems())
    def get_queryset(self):
        self.query = self.request.GET.get('query', '').strip()
        self.mode  = self.MATCH_MODES.get(self.request.GET.get('mode'), 'SPH_MATCH_PHRASE')
        if len(self.query) == 0:
            return ContentItem.objects.none()
        else:
            return ContentItem.search.query(self.query).set_options(mode=self.mode)

    def get_context_data(self, **kwargs):
        context = super(SearchView, self).get_context_data(**kwargs)
        context['mode'] = self.INV_MATCH_MODES.get(self.mode)
        context['query'] = self.query
        return context

search = SearchView.as_view()

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
            zh = ZhivotovIllustration.objects.filter(pub_date__lte = wend).latest('pub_date')
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
        newsletter = cached(get_content, 'newsletter', duration=60*60)
    else:
        newsletter = get_content()
    def get_latest_rubric(rubric, **kwargs):
        try:
            qs = ContentItem.objects.filter(enabled=True, rubric=rubric)
            if kwargs:
                qs = qs.filter(**kwargs)
            return qs.latest('pub_date')
        except ContentItem.DoesNotExist:
            return None
    if not no_cache:
        illustration = cached(get_illustration, 'illustration', duration=6000)
    else:
        illustration = get_illustration()
    def get_zavtra_tv():
        return list( ContentItem.objects.filter(kind='video', rubric=44).order_by('-pub_date')[0:3] )
    #qs = ContentItem.objects.filter(kind='video', rubric=44)
    #most_commented = qs.order_by('-_comments_count')[0]
    #top = list(qs.order_by('-pub_date').exclude(id = most_commented.id)[0:2])
    #return [top[0], most_commented, top[1]]
    zavtra_tv = cached(get_zavtra_tv, 'zavtra-tv', duration=60*60*4)
    special_project = cached(
        lambda: get_latest_rubric(47),
        'special-project',
        duration=60*60*4
    )
    def get_most_commented():
        from django.db import connection
        cursor = connection.cursor()
        cursor.execute("select object_id, count(id) as cmnt from comments_comment where content_type_id=%d and created_at >= date '%s' group by object_id order by cmnt desc limit 5" % (contentitem_ctype_id, now) )
        return list(ContentItem.objects.filter(pk__in=map(lambda w: w[0], cursor.fetchall())))
    most_commented = cached(
        get_most_commented,
        'most-commented',
        duration=60*60
    )
    return {
        'issue_info': { 'date': wstart, 'num': num },
        'newsletter': newsletter,
        'illustration': illustration,
        'zavtra_tv': zavtra_tv,
        'special_project': special_project,
        'most_commented': most_commented
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
    return {'login_fail': False}

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
    cmnt = templateLookup.get_template('base.html').get_def('comment')
    comments = comments[0:qty]
    return {
        'stream': list( reversed(map(lambda comment: cmnt.render_unicode(item=comment, request=request, vote=0, stream=True), comments)) )
    }

@ajax_request
@login_required
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
            if hasattr(obj, 'recalculate_rating'):
                obj.recalculate_rating()
        if request.is_ajax():
            return {'rating': obj.rating, 'vote': vote}
        else:
            return redirect(obj.get_absolute_url())

@render_to('404.html')
def h404(request):
    return {}