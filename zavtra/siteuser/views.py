#-*- coding: utf-8 -*-
from datetime import datetime

from django.shortcuts import get_object_or_404
from django.views.generic import DetailView, ListView

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator
from zavtra.utils import oneday

from siteuser.models import User
from content.models import Article

unneeded_letters = [u'Ъ', u'Ь', u'Ы']
RU_ALPHABET = filter(lambda l: l not in unneeded_letters, map(unichr, range(1040,1072)))

class ProfileView(DetailView):
  template_name = 'siteuser/profile_author.jhtml'
  queryset = User.columnists.prefetch_related('readees','readers').all()

  def get_context_data(self, **kwargs):
    context = super(ProfileView, self).get_context_data(**kwargs)
    context['profile_part'] = 'profile'
    return context


class ArticlesView(ListView):
  template_name = 'siteuser/profile_articles.jhtml'
  paginate_by = 15
  paginator_class = DiggPaginator

  def get_context_data(self, **kwargs):
    context = super(ArticlesView, self).get_context_data(**kwargs)
    context['profile_part'] = 'articles'
    context['object'] = self.user
    return context

  def get_queryset(self):
    self.user = get_object_or_404(User, pk=self.kwargs['pk'])
    return self.user.articles.all()


class CommentsView(ListView):
  template_name = 'siteuser/profile_comments.jhtml'
  paginate_by = 15
  paginator_class = DiggPaginator

  def get_context_data(self, **kwargs):
    context = super(CommentsView, self).get_context_data(**kwargs)
    context['profile_part'] = 'comments'
    context['object'] = self.user
    return context

  def get_queryset(self):
    self.user = get_object_or_404(User, pk=self.kwargs['pk'])
    return self.user.comments.all()



class CommunityView(ListView):
  template_name = 'siteuser/community.jhtml'
  paginate_by = 15
  paginator_class = DiggPaginator
  selected_date = None

  def get_queryset(self):
    qs = Article.published.\
         filter(authors__level__gte = User.USER_LEVELS.trusted).\
         prefetch_related('authors').defer('content')
    if 'year' in self.request.GET and \
       'month' in self.request.GET and \
       'day' in self.request.GET:
       dt = datetime(
        year = int(self.request.GET['year']),
        month = int(self.request.GET['month']),
        day = int(self.request.GET['day']),
        hour = 0, minute = 0, second = 0
       )
       qs = qs.filter(published_at__gte = dt, published_at__lt = dt + oneday)
       self.selected_date = dt
    return qs

  def get_context_data(self, **kwargs):
    context = super(CommunityView, self).get_context_data(**kwargs)
    context['alphabet'] = RU_ALPHABET
    if self.selected_date is not None:
      context['selected_date'] = self.selected_date
    context['most_commented'] = Article.get_most_commented()
    return context
