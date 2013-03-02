#-*- coding: utf-8 -*-
from urllib import urlencode
from datetime import datetime, timedelta
from calendar import isleap
from pytils.dt import MONTH_NAMES
from django.views.generic import DetailView, ListView, TemplateView, RedirectView
from django.shortcuts import get_object_or_404, redirect
from django.db.models import Max, Min, Q, Count

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator,\
                             ExtendedQuerySetDiggPaginator as ExtendedDiggPaginator
from zavtra.utils import oneday

from content.models import Article, Rubric, Topic,\
                           Issue, RubricInIssue, ArticleVote,\
                           SpecialProject
from content.forms import SearchForm

from siteuser.models import User


class EventsView(ListView):
  template_name = 'content/events.jhtml'

  def get_date(self):
    if 'date' in self.kwargs:
      date = datetime.strptime(self.kwargs['date'], '%Y-%m-%d').date()
    else:
      date = datetime.now().date()
    return date

  def get_context_data(self, **kwargs):
    context = super(EventsView, self).get_context_data(**kwargs)
    context['date'] = self.date
    context['prev_date'] = self.date - oneday
    context['next_date'] = self.date + oneday

    dates = Article.common_news.aggregate(start = Min('published_at'), end = Max('published_at'))
    if context['prev_date'] < dates['start'].date(): context['prev_date'] = None
    if context['next_date'] > dates['end'].date(): context['next_date'] = None
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
      related = []
      # TODO: this is nightmare...
      prev = list(reversed(Article.wod.filter(published_at__lt = context['object'].published_at)[0:4]))
      next = list(Article.wod.filter(published_at__gt = context['object'].published_at)[0:4])

      related.append(context['object'])
      while len(related) < 5 and (len(prev) + len(next) > 0):
        try:
          related.append(next.pop())
        except IndexError:
          pass
        try:
          related.insert(0, prev.pop())
        except IndexError:
          pass

      context['related'] = related
    return context

  def get_queryset(self):
    return Article.published.select_related().\
           prefetch_related(
            'expert_comments__expert',
            'topics',
            'authors',
            'cites'
           )


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
    return Article.published.filter(rubric=self.rubric).select_related().\
           prefetch_related('expert_comments__expert')

  def get_context_data(self, **kwargs):
    context = super(RubricView, self).get_context_data(**kwargs)
    self.is_zeitung = self.rubric.from_zeitung
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
    related = []
    # TODO: this is nightmare...
    prev = list(reversed(Issue.published.filter(published_at__lt = context['issue'].published_at)[0:4]))
    next = list(Issue.published.filter(published_at__gt = context['issue'].published_at)[0:4])

    related.append(context['issue'])
    while len(related) < 5:
      try:
        related.append(next.pop())
      except IndexError:
        pass
      try:
        related.insert(0, prev.pop())
      except IndexError:
        pass

    context['related'] = related
    return context


class CommunityView(ListView):
  template_name = 'content/community.jhtml'
  paginate_by = 15
  paginator_class = DiggPaginator
  selected_date = None

  def get_queryset(self):
    now = datetime.now()
    qs = Article.published.\
         filter(authors__level__gte = User.USER_LEVELS.trusted).\
         prefetch_related('authors').defer('content').distinct()
    try:
      p = int(self.request.GET.get('page'))
    except (TypeError, ValueError):
      p = None
    if p is None or p == 1:
      self.newest = list(qs.filter(published_at__gte = now - timedelta(hours=12))[0:2])
    else:
      self.newest = []
    qs = qs.exclude(pk__in = [w.pk for w in self.newest])
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
    context['newest'] = self.newest
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
  paginate_by = 5
  paginator_class = DiggPaginator
  template_name = 'content/search.jhtml'

  def get_queryset(self):
    self.category = self.kwargs.get('category')
    self.found_authors = []

    if self.category == 'authors':
      qs = User.columnists.annotate(articles_count = Count('articles')).\
           annotate(left_comments = Count('comments')).\
           annotate(expert_comments_count = Count('expert_comments'))

    else:
      qs = Article.searcher
    
    # process form
    self.form = SearchForm(self.request.GET)
    if self.form.is_valid():
      data = self.form.cleaned_data
      q = data['query']
      if self.category != 'authors':
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
        qs = qs.filter(
          Q(first_name__icontains = q) | Q(last_name__icontains = q) |
          Q(resume__icontains = q) | Q(bio__icontains = q)
        )
    
    # pack params for GET reuse
    self.form_data = {}
    for k, v in self.form.data.iteritems():
      self.form_data[k] = unicode(v).encode('utf-8')

    # apply rubric filter if any
    if self.category == 'wod':
      qs = qs.filter(rubric=Rubric.fetch_rubric('wod'))
    elif self.category == 'events':
      qs = qs.filter(rubric=Rubric.fetch_rubric('novosti'))
    if self.category == 'authors':
      return qs.all()
    else:
      return qs.select_related().prefetch_related('authors', 'topics').\
             defer('content').order_by('-published_at')

  def get_context_data(self, **kwargs):
    context = super(SearchView, self).get_context_data(**kwargs)
    context['form'] = self.form
    context['found_authors'] = self.found_authors
    context['page_suffix'] = urlencode(self.form_data)
    context['category'] = self.category
    return context


class SpecProjectsView(RedirectView):
  def get(self, request, *args, **kwargs):
    self.url = SpecialProject.objects.filter(date__lte = datetime.now().date()).\
               latest('date').get_absolute_url()
    return super(SpecProjectsView, self).get(request, *args, **kwargs)


class SpecProjectView(DetailView):
  template_name = 'content/special_projects.jhtml'

  def get_context_data(self, **kwargs):
    context = super(SpecProjectView, self).get_context_data(**kwargs)
    context['others'] = SpecialProject.objects.filter(date__lte=self.now).\
                        order_by('date')[0:4]
    return context

  def get_queryset(self):
    self.now = datetime.now().date()
    return SpecialProject.objects.filter(date__lte = self.now).\
           prefetch_related('articles', 'articles__topics')


def current_issue_redirect(request):
  return redirect(Issue.objects.latest('published_at'))