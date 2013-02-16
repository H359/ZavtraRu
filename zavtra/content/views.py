from datetime import datetime
from calendar import isleap
from pytils.dt import MONTH_NAMES
from django.views.generic import DetailView, ListView, TemplateView
from django.shortcuts import get_object_or_404, redirect
from django.db.models import Max, Min

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator
from zavtra.utils import oneday
from content.models import Article, Rubric, Topic, Issue, RubricInIssue


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
    return Article.news.filter(
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
    return context

  def get_queryset(self):
    return Article.objects.select_related()



class RubricView(ListView):
  paginate_by = 5
  paginator_class = DiggPaginator

  @property
  def template_name(self):
    if RubricInIssue.objects.filter(rubric=self.rubric).count() > 0:
      return 'content/issue_rubric_detail.jhtml'
    elif self.rubric.id == Rubric.fetch_rubric('wod').id:
      return 'content/wod.jhtml'
    else:
      return 'content/site_rubric_detail.jhtml'

  def get_queryset(self):
    self.rubric = get_object_or_404(Rubric, slug=self.kwargs['slug'])
    return self.rubric.articles.order_by('-published_at').all()

  def get_context_data(self, **kwargs):
    context = super(RubricView, self).get_context_data(**kwargs)
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
    return self.topic.articles.select_related().all()


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


def current_issue_redirect(request):
  return redirect(Issue.objects.latest('published_at'))