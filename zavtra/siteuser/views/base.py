#-*- coding: utf-8 -*-
from pytils.dt import MONTH_NAMES
from django.views.generic import ListView
from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator

from content.models import Article, Rubric


class FilteredArticlesView(ListView):
  paginate_by = 15
  paginator_class = DiggPaginator

  def get_filters_qs(self):
    result = []
    for k, v in self.filters.items():
      if v != 0:
        result.append('%s=%d' % (k, v))
    return '&'.join(result)

  def get_context_data(self, **kwargs):
    context = super(FilteredArticlesView, self).get_context_data(**kwargs)
    context['categories'] = self.get_categories()
    context['part'] = 'articles'
    context['category'] = self.category
    context['filters'] = self.filters
    context['filters_suffix'] = '&%s' % self.get_filters_qs()
    context['month_names'] = map(lambda w: w[1], MONTH_NAMES)
    return context

  def get_queryset(self):
    self.filters = {
      'year': int(self.request.GET.get('year', 0)),
      'month': int(self.request.GET.get('month', 0))
    }
    self.category = self.kwargs.get('category', None)
    qs = Article.published.select_related().\
         prefetch_related('authors', 'expert_comments', 'topics') 
         #, 'rubric__issue_rubrics')
    for f, v in self.filters.items():
      if v != 0:
        qs = qs.filter(**{'published_at__%s' % f: v})
    if self.category is not None:
      if self.category == 'wod':
        qs = qs.filter(rubric = Rubric.fetch_rubric('wod'))
      else:
        qs = qs.filter(type = getattr(Article.TYPES, self.category))
    return qs
