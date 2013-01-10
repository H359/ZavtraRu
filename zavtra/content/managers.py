from datetime import datetime

from django.db import models
from django.contrib.auth import get_user_model

from zavtra.utils import cached, oneday


class PublishedArticlesManager(models.Manager):
  def get_query_set(self):
    return super(PublishedArticlesManager, self).get_query_set().\
           filter(published_at__lte = datetime.now()).\
           filter(status = self.model.STATUS.ready)


class ZeitungManager(PublishedArticlesManager):
  def get_query_set(self):
    from content.models import Rubric
    zeitung = Rubric.fetch_rubric('zeitung')
    return super(ZeitungManager, self).get_query_set().filter(rubric__in = zeitung)

  def issue(self, year, number):
    npt = datetime(year=year, day=1, month=1) + 7 * oneday * (number - 1)
    return self.get_query_set().filter(published_at__range = (npt, npt + 7 * oneday))

class ColumnsManager(PublishedArticlesManager):
  def get_query_set(self):
    def get_columnists():
      user_model = get_user_model()
      columnists = user_model.objects.filter(level__gte = user_model.USER_LEVELS.columnist)
      return columnists.values('id')
    from content.models import Rubric
    columnists = cached(get_columnists, 'columnists')
    columns = Rubric.fetch_rubric('columns')
    return super(ColumnsManager, self).get_query_set().\
           filter(authors__in = columnists).\
           filter(rubric__in = columns)


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


class EventsManager(PublishedArticlesManager):
  def get_query_set(self):
    from content.models import Rubric
    events = Rubric.fetch_rubric('events')
    return super(EventsManager, self).get_query_set().filter(rubric__in = events)


class WODManager(PublishedArticlesManager):
  def get_query_set(self):
    from content.models import Rubric
    wods = Rubric.fetch_rubric('wod')
    return super(WODManager, self).get_query_set().filter(rubric__in = wods)


class EditorialManager(PublishedArticlesManager):
  def get_query_set(self):
    from content.models import Rubric
    editorial = Rubric.fetch_rubric('editorial')
    return super(EditorialManager, self).get_query_set().filter(rubric__in = editorial)
