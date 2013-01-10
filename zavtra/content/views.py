from datetime import datetime
from calendar import isleap
from django.views.generic import DetailView, ListView
from django.shortcuts import get_object_or_404

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator
from zavtra.utils import oneday
from content.models import Article, Rubric, Topic


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
    context['latest_news'] = Article.objects.filter(rubric__in = Rubric.fetch_rubric('novosti'))
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
    return Article.zeitung.\
           issue(int(self.kwargs['year']), int(self.kwargs['issue'])).\
           select_related().\
           order_by('rubric__position')