from datetime import datetime
from itertools import groupby

from django.http import HttpResponseRedirect
from django.contrib.auth import logout as auth_logout

from annoying.decorators import render_to

from corecontent.models import ContentItem, Issue, Rubric

@render_to('home.html')
def home(request):
    rubrics = Rubric.objects.filter(on_main=True)
    last_issue = Issue.objects.filter(date__lte = datetime.now()).latest('date')
    return {
        'rubrics': rubrics,
        'last_issue': last_issue,
        'blogs_stream': ContentItem.objects.filter(rubric=None, enabled=True)
    }

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
