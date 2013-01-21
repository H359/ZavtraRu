from datetime import datetime
from calendar import isleap
from django.views.generic import DetailView, ListView, TemplateView
from django.views.generic.dates import DayArchiveView
from django.shortcuts import get_object_or_404, redirect

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator
from zavtra.utils import oneday
from content.models import Article, Rubric, Topic, Issue, RubricInIssue


class DayArchiveViewDefaulted(DayArchiveView):
  date_field = 'published_at'
  year_format = '%Y'
  month_format = '%m'
  day_format = '%d'

  def get(self, request, *args, **kwargs):
    if 'year' not in self.kwargs:
      date = getattr(self.get_queryset().latest(self.date_field), self.date_field)
      self.year = '%04d' % date.year
      self.month = '%02d' % date.month
      self.day = '%02d' % date.day
    return super(DayArchiveViewDefaulted, self).get(request, *args, **kwargs)


class EventsView(DayArchiveViewDefaulted):
  template_name = 'content/events.jhtml'
  def get_queryset(self):
    return Article.events.all()

  def get_context_data(self, **kwargs):
    context = super(EventsView, self).get_context_data(**kwargs)
    context['news'] = Article.news.all()[0:4]
    return context


class DailyView(DayArchiveViewDefaulted):
  template_name = 'content/daily.jhtml'
  def get_queryset(self):
    return Article.published.all()

  def get_context_data(self, **kwargs):
    context = super(DailyView, self).get_context_data(**kwargs)
    context['most_commented'] = Article.get_most_commented()
    return context


class ArchiveView(TemplateView):
  template_name = 'content/archive.jhtml'

  def get_context_data(self, **kwargs):
    context = super(ArchiveView, self).get_context_data(**kwargs)
    try:
      date = datetime(
        year=int(self.kwargs['year']),
        month=int(self.kwargs['month']),
        day=int(self.kwargs['day'])
      )
    except (KeyError, ValueError):
      date = datetime.now().date()
    context['selected_date'] = date
    context['issue'] = Issue.objects.filter(published_at__lte=date).latest('published_at')
    return context


class ArticleView(DetailView):

  @property
  def template_name(self):
    if self.object.rubric.id == Rubric.fetch_rubric('wod').id:
      return 'content/wod_article.jhtml'
    elif self.issue is not None:
      return 'content/zeitung_article.jhtml'
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
  paginate_by = 15
  paginator_class = DiggPaginator

  @property
  def template_name(self):
    if RubricInIssue.objects.filter(rubric=self.rubric).count() > 0:
      return 'content/zeitung_rubric_detail.jhtml'
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


class FeaturedView(ListView):
  paginate_by = 15
  paginator_class = DiggPaginator
  template_name = 'content/topic_detail.jhtml'

  def get_context_data(self, **kwargs):
    context = super(FeaturedView, self).get_context_data(**kwargs)
    context['topic'] = self.topic
    # TODO: fix this
    context['most_commented'] = self.topic.articles.all()[0:5]
    return context

  def get_queryset(self):
    self.topic = get_object_or_404(Topic, slug=self.kwargs['slug'])
    return self.topic.articles.select_related().all()


class ZeitungView(TemplateView):
  template_name = 'content/zeitung.jhtml'

  def get_context_data(self, **kwargs):
    context = super(ZeitungView, self).get_context_data(**kwargs)
    context['issue'] = Issue.published.get(
      published_at__year = self.kwargs['year'],
      relative_number = self.kwargs['issue']
    )
    context['latest_issues'] = Issue.published.all()[0:5]
    return context

def current_issue_redirect(request):
  return redirect(Issue.objects.latest('published_at'))