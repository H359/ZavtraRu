from datetime import datetime

from django.http import HttpResponseRedirect
from django.views.generic import ListView
from django.contrib.auth import logout as auth_logout

from corecontent.models import ContentItem, Issue

class HomeView(ListView):
    paginate_by = 10
    template_name = 'home.html'
    queryset = ContentItem.objects.select_related().filter(enabled=True)
    
    def get_context_data(self, **kwargs):
        context = super(HomeView, self).get_context_data(**kwargs)
        context['latest_issue'] = Issue.objects.filter(date__lte = datetime.now()).latest('date')
        return context

home = HomeView.as_view()

def logout(request):
    auth_logout(request)
    return HttpResponseRedirect('/')

def logged_in(request):
    raise Exception, "OLOLO!"
