#-*- coding: utf-8 -*-
from datetime import datetime

from django.shortcuts import get_object_or_404, redirect
from django.views.generic import DetailView, ListView, TemplateView, RedirectView
from django.views.generic.edit import FormView
from django.db.models import Count

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator
from zavtra.utils import oneday
#from filters import Filter, FilterItem

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
      return redirect('home')
    return self.return_form(form)

  def get(self, request, *args, **kwargs):
    return self.return_form(RegisterUserForm())


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
