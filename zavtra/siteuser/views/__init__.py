#-*- coding: utf-8 -*-
"""
from datetime import datetime
from pytils.dt import MONTH_NAMES

from django.contrib import messages
from django.shortcuts import get_object_or_404, redirect
from django.views.generic import DetailView, ListView, TemplateView, RedirectView
from django.views.generic.edit import FormView
from django.db.models import Count, Q

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator
from zavtra.utils import oneday

from content.models import Article, Rubric
from siteuser.models import User, Reader
from siteuser.forms import UserInfoForm, RegisterUserForm
"""
from django.views.generic import ListView
from django.db.models import Count

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator
from siteuser.models import User, Reader

from profile import ProfileView, ProfileArticlesView, ProfileCommentsView
from cabinet import CabinetView, CabinetArticlesView,\
                    CabinetArticlesSubscriptionsView,\
                    CabinetAuthorsSubscriptionsView,\
                    CabinetPostArticleView
from subscribe import SubscribeUserView, UnSubscribeUserView
from register import RegisterView, RegisterDoneView


unneeded_letters = [u'Ъ', u'Ь', u'Ы']
RU_ALPHABET = filter(lambda l: l not in unneeded_letters, map(unichr, range(1040,1072)))

class AuthorsView(ListView):
  template_name = 'siteuser/authors.jhtml'
  paginate_by = 5
  paginator_class = DiggPaginator

  letter = None
  query = None

  def get_context_data(self, **kwargs):
    from content.models import Article
    context = super(AuthorsView, self).get_context_data(**kwargs)
    context['alphabet'] = RU_ALPHABET
    context['most_commented'] = Article.get_most_commented()
    if self.query is not None:
      context['query'] = self.query
    elif self.letter is not None:
      context['letter'] = self.letter
    if self.request.user is not None and self.request.user.is_authenticated():
      context['user_reads'] = Reader.objects.filter(
        author__in=context['object_list'],
        reader = self.request.user
      ).values_list('author_id', flat=True)
    return context

  def get_queryset(self):
    kwargs = {}
    if 'query' in self.request.GET:
      query = self.request.GET['query']
      self.query = query
      kwargs['last_name__icontains'] = query
      kwargs['first_name__icontains'] = query
    else:
      query = self.request.GET.get('letter', u'А')
      self.letter = query
      kwargs['last_name__istartswith'] = query
    return User.columnists.filter(**kwargs).\
           annotate(articles_count = Count('articles')).\
           annotate(left_comments = Count('comments')).\
           annotate(expert_comments_count = Count('expert_comments'))
