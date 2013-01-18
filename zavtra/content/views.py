from datetime import datetime
from calendar import isleap
from django.views.generic import DetailView, ListView
from django.views.generic.dates import DayArchiveView
from django.shortcuts import get_object_or_404, redirect

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator
from zavtra.utils import oneday
from content.models import Article, Rubric, Topic, ZhivotovIllustration


class DayArchiveViewDefaulted(DayArchiveView):
  date_field = 'published_at'
  year_format = '%Y'
  month_format = '%m'
  day_format = '%d'

  def get(self, request, *args, **kwargs):
    if 'year' not in self.kwargs:
      date = getattr(self.queryset.latest(self.date_field), self.date_field)
      self.year = '%04d' % date.year
      self.month = '%02d' % date.month
      self.day = '%02d' % date.day
    return super(DayArchiveViewDefaulted, self).get(request, *args, **kwargs)


class EventsView(DayArchiveViewDefaulted):
  template_name = 'content/events.jhtml'
  queryset = Article.events.all()

  def get_context_data(self, **kwargs):
    context = super(EventsView, self).get_context_data(**kwargs)
    context['news'] = Article.news.all()[0:4]
    return context

class DailyView(DayArchiveViewDefaulted):
  template_name = 'content/daily.jhtml'
  queryset = Article.columns.all()


class ArchiveView(ListView):
  template_name = 'content/archive.jhtml'

  def get_context_data(self, **kwargs):
    context = super(ArchiveView, self).get_context_data(**kwargs)
    context['selected_year'] = self.date.year
    context['years'] = xrange(1996, datetime.now().year+1)
    obj = context['object_list'][0]
    context['issue'] = {
      'number': obj.issue_number,
      'date': obj.issue_date,
      'others': []
    }
    context['illustration'] = get_illustration(obj.issue_date)
    return context

  def get_queryset(self):
    self.date = self.kwargs.get('date')
    if self.date is None:
      self.date = datetime.now().date() - 48*oneday
    return Article.zeitung.issue_by_date(self.date).select_related()


class ArticleView(DetailView):
  @property
  def template_name(self):
    if self.object.from_wod:
      return 'content/wod_article.jhtml'
    else:
      return 'content/article_detail.jhtml'

  def get_context_data(self, **kwargs):
    context = super(ArticleView, self).get_context_data(**kwargs)
    # pass news qs to view
    # context['latest_news'] = Article.objects.filter(rubric__in = Rubric.fetch_rubric('novosti'))
    return context

  def get_queryset(self):
    return Article.objects.select_related()


class RubricView(ListView):
  paginate_by = 15
  paginator_class = DiggPaginator

  @property
  def template_name(self):
    if self.rubric.zeitung_rubric:
      return 'content/zeitung_rubric_detail.jhtml'
    elif self.rubric.wod_rubric:
      return 'content/wod.jhtml'
    else:
      return 'content/site_rubric_detail.jhtml'

  def normalize_number(self, year, number):
    num = (number % (52 + (1 if isleap(year) else 0)))
    return num if num >= number else num + 1

  def get_context_data(self, **kwargs):
    now = datetime.now().date()
    context = super(RubricView, self).get_context_data(**kwargs)
    context['rubric'] = self.rubric
    if self.rubric.zeitung_rubric:
      number = Article.get_current_issue_number()
      date = Article.get_current_issue_date_range()[0]
      context['issue'] = {
        'number': number,
        'date': date,
        'others': []
      }
      max_positive_shift = (now - context['issue']['date']).days / 7
      max_negative_shift = 5 - max_positive_shift
      for x in range(-max_negative_shift, max_positive_shift):
        date = context['issue']['date'] + x * 7 * oneday
        if date <= now:
          number = context['issue']['number'] + x
          context['issue']['others'].append({
            'date': date,
            'number': self.normalize_number(context['issue']['date'].year, number)
          })
    return context

  def get_queryset(self):
    self.rubric = get_object_or_404(Rubric, slug=self.kwargs['slug'])
    return self.rubric.articles.order_by('-published_at').all()


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


class ZeitungView(ListView):
  template_name = 'content/zeitung.jhtml'

  # FIXME: Evil hack here
  def normalize_number(self, year, number):
    num = (number % (52 + (1 if isleap(year) else 0)))
    return num if num >= number else num + 1


  def get_context_data(self, **kwargs):
    now = datetime.now().date()
    context = super(ZeitungView, self).get_context_data(**kwargs)
    obj = context['object_list'][0]
    context['issue'] = {
      'number': obj.issue_number,
      'date': obj.issue_date,
      'others': []
    }
    context['illustration'] = get_illustration(obj.issue_date)
    max_positive_shift = (now - context['issue']['date']).days / 7
    max_negative_shift = 5 - max_positive_shift
    dates = []
    for x in range(-max_negative_shift, max_positive_shift):
      date = context['issue']['date'] + x * 7 * oneday
      if date <= now:
        number = context['issue']['number'] + x
        context['issue']['others'].append({
          'date': date,
          'number': self.normalize_number(context['issue']['date'].year, number)
        })
        dates.append(date)
    # TODO: rewrite this
    for illustration in ZhivotovIllustration.objects.filter(published_at__range = (dates[0], dates[-1])):
      for d in context['issue']['others']:
        if d['date'] == illustration.published_at.date():
          d['illustration'] = illustration
    return context

  def get_queryset(self):
    return Article.zeitung.\
           issue(int(self.kwargs['year']), int(self.kwargs['issue'])).\
           select_related().\
           order_by('rubric__position')


def get_illustration(date):
  try:
    return ZhivotovIllustration.object.filter(published_at__lt = date).\
           latest('published_at')
  except:
    return None


def current_issue_redirect(request):
  drange = Article.get_current_issue_date_range()
  number = Article.get_current_issue_number()
  return redirect('content.views.zeitung', year=drange[0].year, issue=number)