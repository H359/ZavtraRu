from haystack import indexes
from models import Article


class ArticleIndex(indexes.SearchIndex, indexes.Indexable):
  text     = indexes.CharField(document=True, use_template=True)
  pub_date = indexes.DateTimeField(model_attr='published_at')
  rubric   = indexes.CharField(model_attr='rubric__slug')

  def get_model(self):
    return Article

  def read_queryset(self, using=None):
    return super(ArticleIndex, self).read_queryset(using).\
           select_related().\
           prefetch_related('topics')


  def index_queryset(self, using=None):
    return self.get_model().published.all()