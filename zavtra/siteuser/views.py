#-*- coding: utf-8 -*-
from datetime import datetime

from django.shortcuts import get_object_or_404, redirect
from django.views.generic import DetailView, ListView, TemplateView
from django.views.generic.edit import FormView

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator
from zavtra.utils import oneday
#from filters import Filter, FilterItem

from siteuser.models import User
from siteuser.forms import RegisterUserForm
from content.models import Article

unneeded_letters = [u'Ъ', u'Ь', u'Ы']
RU_ALPHABET = filter(lambda l: l not in unneeded_letters, map(unichr, range(1040,1072)))


class RegisterView(TemplateView, FormView):
  template_name = 'siteuser/register.jhtml'

  def return_form(self, form):
    return self.render_to_response(self.get_context_data(form=form))

  def post(self, request, *args, **kwargs):
    form = RegisterUserForm(request.POST)
    if form.is_valid():
      return redirect('home')
    return self.return_form(form)

  def get(self, request, *args, **kwargs):
    return self.return_form(RegisterUserForm())



class ProfileView(DetailView):
  template_name = 'siteuser/profile_author.jhtml'
  queryset = User.columnists.prefetch_related('readees','readers').all()

  def get_context_data(self, **kwargs):
    context = super(ProfileView, self).get_context_data(**kwargs)
    context['profile_part'] = 'profile'
    return context


"""
class ArticlesFilter(Filter):
  year = FilterItem(title=u'Год', blank=True, blank_string=u'все', field='published_at')
  month = FilterItem(title=u'Месяц', blank=True, blank_string=u'все', field='published_at')
  category = FilterItem(title=u'Категория', blank=True, blank_string=u'все',
    choices=(
      (u'статьи', lambda qs: qs.exclude(rubric__slug = 'wod', type=Article.TYPES.video)),
      (u'видео', lambda qs: qs.filter(type=Article.TYPES.video)),
      (u'слово дня', lambda qs: qs.filter(rubric__slug = 'wod'))
    )
  )
"""


class ArticlesView(ListView):
  template_name = 'siteuser/profile_articles.jhtml'
  paginate_by = 15
  paginator_class = DiggPaginator

  def get_context_data(self, **kwargs):
    context = super(ArticlesView, self).get_context_data(**kwargs)
    context['profile_part'] = 'articles'
    context['object'] = self.user
    #context['filter'] = self.filter
    return context

  def get_queryset(self):
    self.user = get_object_or_404(User, pk=self.kwargs['pk'])
    #self.filter = ArticlesFilter(request=self.request, queryset=self.user.articles.all())
    #return self.filter.as_queryset()
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
