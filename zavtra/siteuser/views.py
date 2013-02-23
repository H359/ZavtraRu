#-*- coding: utf-8 -*-
from datetime import datetime
from pytils.dt import MONTH_NAMES

from django.shortcuts import get_object_or_404, redirect
from django.views.generic import DetailView, ListView, TemplateView, RedirectView
from django.views.generic.edit import FormView
from django.db.models import Count, Q

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator
from zavtra.utils import oneday

from content.models import Article, Rubric
from siteuser.models import User, Reader
from siteuser.forms import RegisterUserForm

unneeded_letters = [u'Ъ', u'Ь', u'Ы']
RU_ALPHABET = filter(lambda l: l not in unneeded_letters, map(unichr, range(1040,1072)))


class SubscribeUser(RedirectView):
  def get(self, request, *args, **kwargs):
    self.url = request.GET.get('next', '/')
    if request.user is not None and request.user.is_authenticated():
      readee = get_object_or_404(User, pk=kwargs['readee'])
      rdr = Reader.objects.filter(author=readee, reader=request.user)
      if rdr.count() == 0:
        Reader.objects.create(author=readee, reader=request.user, subscription_start=datetime.now())
    return super(SubscribeUser, self).get(request, *args, **kwargs)


class UnSubscribeUser(RedirectView):
  def get(self, request, *args, **kwargs):
    self.url = request.GET.get('next', '/')
    if request.user is not None and request.user.is_authenticated():
      Reader.objects.filter(author_id=kwargs['readee'], reader=request.user).delete()
    return super(UnSubscribeUser, self).get(request, *args, **kwargs)


class RegisterView(TemplateView, FormView):
  template_name = 'siteuser/register.jhtml'

  def return_form(self, form):
    return self.render_to_response(self.get_context_data(form=form))

  def post(self, request, *args, **kwargs):
    form = RegisterUserForm(request.POST)
    if form.is_valid():
      data = form.cleaned_data
      user = User.objects.create_user(email=data['email'], password=data['password1'])
      user.first_name = data.get('first_name')
      user.last_name = data.get('last_name')
      user.save()
      return redirect('siteuser.views.register_done')
    return self.return_form(form)

  def get(self, request, *args, **kwargs):
    return self.return_form(RegisterUserForm())


class RegisterDoneView(TemplateView):
  template_name = 'siteuser/register_done.jhtml'


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
    else:
      query = self.request.GET.get('letter', u'А')
      self.letter = query
      kwargs['last_name__istartswith'] = query
    return User.columnists.filter(**kwargs).\
           annotate(articles_count = Count('articles')).\
           annotate(left_comments = Count('comments')).\
           annotate(expert_comments_count = Count('expert_comments'))


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

  def get_filters_qs(self):
    result = []
    for k, v in self.filters.items():
      if v != 0:
        result.append('%s=%d' % (k, v))
    return '&'.join(result)

  def get_context_data(self, **kwargs):
    context = super(ArticlesView, self).get_context_data(**kwargs)
    context['profile_part'] = 'articles'
    context['category'] = self.category
    context['object'] = self.user
    context['filters'] = self.filters
    context['filters_suffix'] = '&%s' % self.get_filters_qs()
    context['month_names'] = map(lambda w: w[1], MONTH_NAMES)
    return context

  def get_queryset(self):
    self.user = get_object_or_404(User, pk=self.kwargs['pk'])
    self.filters = {
      'year': int(self.request.GET.get('year', 0)),
      'month': int(self.request.GET.get('month', 0))
    }
    self.category = self.kwargs.get('category', None)
    queryset = Article.published.select_related().\
               prefetch_related('authors', 'expert_comments', 'topics').\
               filter(Q(authors__in = [self.user]) | Q(expert_comments__expert = self.user))
    for f, v in self.filters.items():
      if v != 0:
        queryset = queryset.filter(**{'published_at__%s' % f: v})
    if self.category is not None:
      if self.category == 'wod':
        queryset = queryset.filter(rubric = Rubric.fetch_rubric('wod'))
      else:
        queryset = queryset.filter(type = getattr(Article.TYPES, self.category))
    return queryset


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
