#-*- coding: utf-8 -*-
from urllib import urlencode
from datetime import datetime
from calendar import isleap
from pytils.dt import MONTH_NAMES
from django.views.generic import DetailView, ListView, TemplateView, RedirectView
from django.shortcuts import get_object_or_404, redirect
from django.db.models import Max, Min, Q

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator,\
                             ExtendedQuerySetDiggPaginator as ExtendedDiggPaginator
from zavtra.utils import oneday

from content.models import Article, Rubric, Topic, Issue, RubricInIssue, ArticleVote
from content.forms import SearchForm

from siteuser.models import User


class EventsView(ListView):
  template_name = 'content/events.jhtml'

  def get_date(self):
    if 'date' in self.kwargs:
      date = datetime.strptime(self.kwargs['date'], '%d-%m-%Y').date()
    else:
      date = datetime.now().date()
    return date

  def get_context_data(self, **kwargs):
    context = super(EventsView, self).get_context_data(**kwargs)
    context['date'] = self.date    
    context['prev_date'] = self.date - oneday
    context['next_date'] = self.date + oneday

    dates = Article.news.aggregate(start = Min('published_at'), end = Max('published_at'))
    if context['prev_date'] < dates['start'].date():
      context['prev_date'] = None
    if context['next_date'] > dates['end'].date():
      context['next_date'] = None

    context['latest_events'] = Article.events.all()[0:5]
    return context

  def get_queryset(self):
    self.date = self.get_date()
    return Article.common_news.filter(
      published_at__year = self.date.year,
      published_at__month = self.date.month,
      published_at__day = self.date.day
    ).select_related().prefetch_related('topics')


class ArchiveView(TemplateView):
  template_name = 'content/archive.jhtml'

  def get_context_data(self, **kwargs):
    context = super(ArchiveView, self).get_context_data(**kwargs)
    queryset = Issue.objects.order_by('-published_at')
    # TODO: refactor this SHIT
    latest_issue = Issue.objects.latest('published_at')
    if 'year' in self.request.GET:
      context['selected_year'] = int(self.request.GET['year'])
    else:
      context['selected_year'] = latest_issue.published_at.year
    queryset = queryset.filter(published_at__year = context['selected_year'])
    if 'month' in self.request.GET:
      context['selected_month'] = int(self.request.GET['month'])
      queryset = queryset.filter(published_at__month = context['selected_month'])
    else:
      context['selected_month'] = 0
    context['built_attrs'] = []
    if 'selected_year' in context:
      context['built_attrs'].append('year=%d' % context['selected_year'])
    if 'selected_month' in context and context['selected_month'] != 0:
      context['built_attrs'].append('month=%d' % context['selected_month'])
    context['built_attrs'] = '&'.join(context['built_attrs'])
    if 'number' in self.request.GET:
      context['number'] = int(self.request.GET['number'])
    context['issues'] = queryset
    context['months'] = [x[1] for x in MONTH_NAMES]
    return context


class ArticleView(DetailView):
  @property
  def template_name(self):
    if self.object.rubric.id == Rubric.fetch_rubric('wod').id:
      return 'content/wod_article.jhtml'
    elif self.object.rubric.id == Rubric.fetch_rubric('novosti').id:
      return 'content/event_article.jhtml'
    elif self.issue is not None:
      return 'content/issue_article.jhtml'
    else:
      return 'content/site_article.jhtml'

  def get_context_data(self, **kwargs):
    context = super(ArticleView, self).get_context_data(**kwargs)
    self.issue = self.object.issue
    context['issue'] = self.issue
    if self.request.user and self.request.user.is_authenticated():
      try:
        vote = context['object'].votes.get(user=self.request.user)
        context['has_voted'] = True
        context['vote'] = vote
      except ArticleVote.DoesNotExist:
        context['has_voted'] = False
    if self.object.rubric.id == Rubric.fetch_rubric('wod').id:
      context['related'] = Article.published.defer('content')[0:5]
    return context

  def get_queryset(self):
    return Article.objects.select_related().\
           prefetch_related('expert_comments', 'expert_comments__expert', 'cites')



class RubricView(ListView):
  paginate_by = 5
  paginator_class = DiggPaginator
  is_zeitung = False

  @property
  def template_name(self):
    if self.is_zeitung:
      return 'content/issue_rubric_detail.jhtml'
    elif self.rubric.id == Rubric.fetch_rubric('wod').id:
      return 'content/wod.jhtml'
    else:
      return 'content/site_rubric_detail.jhtml'

  def get_queryset(self):
    self.rubric = get_object_or_404(Rubric, slug=self.kwargs['slug'])
    #return self.rubric.articles.order_by('-published_at').all()
    return Article.published.filter(rubric=self.rubric)

  def get_context_data(self, **kwargs):
    context = super(RubricView, self).get_context_data(**kwargs)
    qs = RubricInIssue.objects.filter(rubric=self.rubric)
    self.is_zeitung = qs.count() > 0
    context['rubric'] = self.rubric
    return context


class TopicView(ListView):
  paginate_by = 15
  paginator_class = DiggPaginator
  template_name = 'content/topic_detail.jhtml'

  def get_context_data(self, **kwargs):
    context = super(TopicView, self).get_context_data(**kwargs)
    context['topic'] = self.topic
    # TODO: fix this
    context['most_commented'] = self.topic.articles.all()[0:5]
    return context

  def get_queryset(self):
    self.topic = get_object_or_404(Topic, slug=self.kwargs['slug'])
    #return self.topic.articles.select_related().all()
    return Article.published.filter(topics__in = [self.topic]).select_related().\
           prefetch_related('authors', 'topics')


class IssueView(TemplateView):
  template_name = 'content/issue.jhtml'

  def get_context_data(self, **kwargs):
    context = super(IssueView, self).get_context_data(**kwargs)
    context['issue'] = Issue.published.get(
      published_at__year = self.kwargs['year'],
      relative_number = self.kwargs['issue']
    )
    context['latest_issues'] = Issue.published.all()[0:5]
    return context


class CommunityView(ListView):
  template_name = 'content/community.jhtml'
  paginate_by = 15
  #paginator_class = DiggPaginator
  paginator_class = ExtendedDiggPaginator
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
    if self.selected_date is not None:
      context['selected_date'] = self.selected_date
    context['most_commented'] = Article.get_most_commented()
    return context


class ArticleVoteView(RedirectView):
  def get(self, request, *args, **kwargs):
    self.url = '/'
    if request.user is not None and request.user.is_authenticated():
      obj = Article.published.get(slug=self.kwargs['slug'])
      self.url = obj.get_absolute_url()
      vote, _ = ArticleVote.objects.get_or_create(user=request.user, article=obj)
      vote.vote = 1 if self.kwargs['vote'] == 'up' else -1
      vote.save()
    return super(ArticleVoteView, self).get(request, *args, **kwargs)


class SearchView(ListView):
  paginate_by = 15
  paginator_class = DiggPaginator
  template_name = 'content/search.jhtml'

  def get_queryset(self):
    qs = Article.searcher

    # process form
    self.form = SearchForm(self.request.GET)
    if self.form.is_valid():
      data = self.form.cleaned_data
      q = data['query']
      qs = qs.search(query=q, rank_field='rank')
      if data['start']:
        qs = qs.filter(published_at__gte = data['start'])
      if data['end']:
        qs = qs.filter(published_at__lte = data['end'])
      self.found_authors = User.columnists.filter(
        Q(first_name__icontains = q) | Q(last_name__icontains = q) |
        Q(resume__icontains = q) | Q(bio__icontains = q)
      )
    else:
      self.found_authors = []
    
    # pack params for GET reuse
    self.form_data = {}
    for k, v in self.form.data.iteritems():
      self.form_data[k] = unicode(v).encode('utf-8')

    # apply rubric filter if any
    self.category = self.kwargs.get('category')
    if self.category == 'wod':
      qs = qs.filter(rubric=Rubric.fetch_rubric('wod'))
    elif self.category == 'events':
      qs = qs.filter(rubric=Rubric.fetch_rubric('novosti'))
    return qs.select_related().prefetch_related('authors', 'topics').\
           defer('content').order_by('-published_at')

  def get_context_data(self, **kwargs):
    context = super(SearchView, self).get_context_data(**kwargs)
    context['form'] = self.form
    context['found_authors'] = self.found_authors
    context['page_suffix'] = urlencode(self.form_data)
    context['category'] = self.category
    return context


def current_issue_redirect(request):
  return redirect(Issue.objects.latest('published_at'))