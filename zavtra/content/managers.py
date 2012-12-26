from datetime import datetime

from django.db import models
from django.contrib.auth import get_user_model

from zavtra.utils import cached


class PublishedArticlesManager(models.Manager):
  def get_query_set(self):
    return super(PublishedArticlesManager, self).get_query_set().\
           filter(published_at__lte = datetime.now()).\
           filter(status = self.model.STATUS.ready)


class GazetteManager(PublishedArticlesManager):
  def get_query_set(self):
    from content.models import Rubric
    # TODO: cache this
    gazette = Rubric.objects.get(slug = 'gazeta').get_descendants(include_self = True)
    return super(GazetteManager, self).get_query_set().filter(rubric__in = gazette)


class ColumnsManager(PublishedArticlesManager):
  def get_query_set(self):
    def get_columnists():
      user_model = get_user_model()
      columnists = user_model.objects.filter(level__gte = user_model.USER_LEVELS.columnist)
      return columnists.values('id')
    columnists = cached(get_columnists, 'columnists')
    return super(ColumnsManager, self).get_query_set().filter(authors__in = columnists)


class BlogsManager(PublishedArticlesManager):
  def get_query_set(self):
    from content.models import Rubric
    blogs = Rubric.fetch_rubric('blogi')
    return super(BlogsManager, self).get_query_set().filter(rubric__in = blogs)


class NewsManager(PublishedArticlesManager):
  def get_query_set(self):
    from content.models import Rubric
    news = Rubric.fetch_rubric('novosti')
    return super(NewsManager, self).get_query_set().filter(rubric__in = news)


class WODManager(PublishedArticlesManager):
  def get_query_set(self):
    from content.models import Rubric
    wods = Rubric.fetch_rubric('wod')
    return super(WODManager, self).get_query_set().filter(rubric__in = wods)
