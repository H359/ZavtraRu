#-*- coding: utf-8 -*-
from datetime import datetime

from django.shortcuts import get_object_or_404
from django.views.generic import DetailView, ListView
from django.views.generic.dates import DayArchiveView

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator

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


class CommunityView(DayArchiveView):
  allow_empty = True
  template_name = 'siteuser/community.jhtml'
  date_field = 'published_at'
  year_format = '%Y'
  month_format = '%m'
  day_format = '%d'


  def get_context_data(self, **kwargs):
    context = super(CommunityView, self).get_context_data(**kwargs)
    context['alphabet'] = RU_ALPHABET
    context['selected_letter'] = self.letter
    context['most_commented'] = Article.get_most_commented()
    return context

  def dispatch(self, request, *args, **kwargs):
    if 'year' not in self.kwargs:
      now = datetime.now()
      self.kwargs['year'] = u'%04d' % now.year
      self.kwargs['month'] = u'%02d' % now.month
      self.kwargs['day'] = u'%02d' % now.day
    return super(CommunityView, self).dispatch(request, *args, **kwargs)

  def get_queryset(self):
    self.letter = self.request.GET.get('letter')
    if self.letter is None:
      self.letter = u"А"
    print self.letter
    return Article.objects.prefetch_related('authors').\
           filter(
              authors__last_name__istartswith = self.letter,
              status = Article.STATUS.ready
           ).\
           select_related().defer('content')
