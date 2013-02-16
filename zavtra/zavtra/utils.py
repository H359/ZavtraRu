from datetime import timedelta
from django.core.cache import cache
from django.db.models import Model


oneday = timedelta(days=1)


Model.provides_open_graph = False


class OpenGraphMixin(object):
  provides_open_graph = True

  @property
  def open_graph_data(self):
    raise NotImplementedError('No open_graph_data implementation for %s' % self.__class__.__name__)


def cached(func, key, duration=60):
  res = cache.get(key)
  if res is None:
    res = func()
    cache.set(key, res, duration)
  return res