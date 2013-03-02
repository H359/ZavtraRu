# -*- coding: utf-8 -*-
from datetime import datetime
from jinja2.ext import Extension
from django.core.cache import cache
from django_jinja.base import Library
from django.utils.datastructures import SortedDict
import jinja2
from pytils.dt import ru_strftime, distance_of_time_in_words
from pytils.numeral import get_plural as _get_plural
from content.models import Issue

register = Library()

@register.filter
@jinja2.contextfilter
def get_plural(ctx, value, variants):
  return _get_plural(value, variants)

@register.filter
@jinja2.contextfilter
def ru_dt_distance(ctx, value, accuracy=1):
  return distance_of_time_in_words(value, accuracy=accuracy)

@register.filter
@jinja2.contextfilter
def ru_dt_distance_fuzzy(ctx, value):
  diff = (datetime.now().date() - value.date()).days
  if diff > 1:
    return ru_strftime(unicode(u'%d %B %Y г'), value, inflected=True)
  elif diff == 1:
    return u'Вчера'
  else:
    return u'Сегодня'


@register.filter
@jinja2.contextfilter
def ru_date(ctx, value, format=u'%d %B %Y г', inflected=True):
  return ru_strftime(unicode(format), value, inflected=inflected)

@register.filter
@jinja2.contextfilter
def nl2br(ctc, value):
  return u'<br>'.join(value.split('\n'))

@register.filter
@jinja2.contextfilter
def group_rubrics(ctx, value):
  dct = SortedDict()
  for item in value:
    if item.rubric_id in dct:
      dct[item.rubric_id]['items'].append(item)
    else:
      dct[item.rubric_id] = {'items': [item], 'rubric': item.rubric}
  num = 0
  rubrics = dct.values()
  nlen = len(rubrics)
  while nlen > num:
    if len(rubrics[num]['items']) == 1 and nlen > num + 1 and len(rubrics[num + 1]['items']) == 1:
      yield True, (rubrics[num], rubrics[num + 1])
      num += 2
    else:
      yield False, (rubrics[num],)
      num += 1

@register.filter
@jinja2.contextfilter
def get_issues_from_list(ctx, value, rubric):
  articles = sorted(map(lambda p: p.published_at.date(), value))
  issues = Issue.published.filter(published_at__range = (articles[0], articles[-1]))
  for issue in issues:
    group = {
      'rubric': rubric,
      'items': [article for article in value if article.published_at.date() == issue.published_at]
    }
    if len(group['items']) == 0: continue
    yield (issue, [group])


class CachedExtension(Extension):
  tags = set(['cached'])

  def parse(self, parser):
    lineno = next(parser.stream).lineno
    duration = parser.parse_expression()
    name = parser.parse_expression()
    body = parser.parse_statements(['name:endcached'], drop_needle=True)
    return jinja2.nodes.CallBlock(
      self.call_method('_cache_support', [name, duration]),
      [], [], body
    ).set_lineno(lineno)

  def _cache_support(self, name, duration, caller):
    key = 'templates:%s' % name
    rv = cache.get(key)
    if rv is None:
      rv = caller()
      cache.set(key, rv, duration)
    return rv