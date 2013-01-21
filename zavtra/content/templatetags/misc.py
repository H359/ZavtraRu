# -*- coding: utf-8 -*-
from django_jinja.base import Library
from django.utils.datastructures import SortedDict
import jinja2
from pytils.dt import ru_strftime, distance_of_time_in_words
from pytils.numeral import get_plural as _get_plural

register = Library()

@register.filter
@jinja2.contextfilter
def get_plural(ctx, value, variants):
  return _get_plural(value, variants)

@register.filter
@jinja2.contextfilter
def ru_dt_distance(ctx, value):
  return distance_of_time_in_words(value)

@register.filter
@jinja2.contextfilter
def ru_date(ctx, value, format=u'%d %B %Y г'):
  return ru_strftime(unicode(format), value, inflected=True)


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
