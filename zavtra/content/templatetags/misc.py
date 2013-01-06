# -*- coding: utf-8 -*-
from django_jinja.base import Library
import jinja2
from pytils.dt import ru_strftime, distance_of_time_in_words


register = Library()

@register.filter
@jinja2.contextfilter
def ru_dt_distance(ctx, value):
  return distance_of_time_in_words(value)

@register.filter
@jinja2.contextfilter
def ru_date(ctx, value):
  return ru_strftime(u'%d %B %Y г', value, inflected=True)


@register.filter
@jinja2.contextfilter
def group_rubrics(ctx, value):
  rubrics = {}
  for item in value:
    rubrics.setdefault(item.rubric_id, {'rubric': None, 'items': []})
    if rubrics[item.rubric_id]['rubric'] is None:
      rubrics[item.rubric_id]['rubric'] = item.rubric
    rubrics[item.rubric_id]['items'].append(item)
  rubrics = sorted(rubrics.values(), key=lambda p: p['rubric'].position)
  num = 0
  nlen = len(rubrics)
  while nlen > num:
    if len(rubrics[num]['items']) == 1 and nlen > num + 1 and len(rubrics[num + 1]['items']) == 1:
      yield True, (rubrics[num], rubrics[num + 1])
      num += 2
    else:
      yield False, (rubrics[num],)
      num += 1
