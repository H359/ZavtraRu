from datetime import datetime

from django.db import models
from django.db.models import Q
from django.contrib.auth import get_user_model

from zavtra.utils import cached, oneday


class PublishedManager(models.Manager):
  def get_query_set(self):
    qs = super(PublishedManager, self).get_query_set().\
         filter(published_at__lte = datetime.now())
    if hasattr(self.model, 'STATUS'):
      qs = qs.filter(status = self.model.STATUS.ready)
    return qs


class BaseNewsManager(PublishedManager):
  def get_query_set(self):
    from content.models import Rubric
    return super(BaseNewsManager, self).get_query_set().\
           filter(rubric=Rubric.fetch_rubric('novosti'))


class EventsManager(BaseNewsManager):
  def get_query_set(self):
    from content.models import Rubric
    return super(EventsManager, self).get_query_set().\
           filter(rubric=Rubric.fetch_rubric('novosti')).\
           exclude(Q(cover_source = "") | Q(cover_source__isnull = True))


class NewsManager(BaseNewsManager):
  def get_query_set(self):
    from content.models import Rubric
    return super(NewsManager, self).get_query_set().\
           filter(rubric=Rubric.fetch_rubric('novosti')).\
           filter(Q(cover_source = "") | Q(cover_source__isnull = True))


class WODManager(PublishedManager):
  def get_query_set(self):
    from content.models import Rubric
    return super(WODManager, self).get_query_set().\
           filter(rubric=Rubric.fetch_rubric('wod'))


class ColumnsManager(PublishedManager):
  def get_query_set(self):
    from content.models import Rubric
    return super(ColumnsManager, self).get_query_set().\
           filter(rubric=Rubric.fetch_rubric('columnists'))


class EditorialManager(PublishedManager):
  def get_query_set(self):
    from content.models import Rubric
    return super(EditorialManager, self).get_query_set().\
           filter(rubric=Rubric.fetch_rubric('editorial'))