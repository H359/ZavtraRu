from datetime import datetime
from itertools import groupby

from django.http import HttpResponseRedirect
from django.views.generic import ListView
from django.contrib.auth import logout as auth_logout

from annoying.decorators import render_to

from corecontent.models import ContentItem, Issue

class HomeView(ListView):
    paginate_by = 10
    template_name = 'home.html'
    kind = None
    
    def get_queryset(self):
        qs = ContentItem.objects.select_related().filter(enabled=True)
        if len(self.args) > 0:
            self.kind = self.args[0]
            qs = qs.filter(kind=self.kind)
        return qs
    
    def get_context_data(self, **kwargs):
        context = super(HomeView, self).get_context_data(**kwargs)
        context['latest_issue'] = Issue.objects.filter(date__lte = datetime.now()).latest('date')
        context['kind'] = self.kind
        return context

home = HomeView.as_view()

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
