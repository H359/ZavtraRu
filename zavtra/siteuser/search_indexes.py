"""
from haystack import indexes
from models import User

class UserIndex(indexes.SearchIndex, indexes.Indexable):
  text     = indexes.CharField(document=True, use_template=False)

  def get_model(self):
    return User

  def index_queryset(self, using=None):
    return self.get_model().objects.filter(level__gt = 0)
"""