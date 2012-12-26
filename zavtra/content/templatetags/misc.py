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
  return ru_strftime(u'%d %B', value)