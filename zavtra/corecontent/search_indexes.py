from datetime import datetime

from django.utils.html import strip_tags

from haystack import indexes
from models import ContentItem

class ContentItemIndex(indexes.RealTimeSearchIndex, indexes.Indexable):
    KIND_MAP    = {'text': 1, 'video': 2, 'image': 3}
    title       = indexes.CharField(boost=1.5, model_attr='title')
    authors     = indexes.FacetMultiValueField(boost=1.25)
    pub_date    = indexes.DateField(indexed=False, model_attr='pub_date')
    kind        = indexes.IntegerField(indexed=False)
    link        = indexes.CharField(stored=True, indexed=False, model_attr='get_absolute_url')
    text        = indexes.CharField(document=True, model_attr='content')

    def prepare_kind(self, obj):
	return self.KIND_MAP.get(obj.kind, 0)

    def get_model(self):
	return ContentItem

    def index_queryset(self):
	now = datetime.now()
	return self.get_model().batched.batch_select('authors').filter(enabled=True, pub_date__lte = now)

    def should_update(self, instance, **kwargs):
	now = datetime.now().date()
	return instance.enabled and instance.pub_date <= now

    def prepare_title(self, obj):
	return strip_tags(obj.title)

    def prepare_text(self, obj):
	return strip_tags(obj.content)

    def prepare_authors(self, obj):
	if hasattr(obj, 'authors_all'):
	    authors = obj.authors_all
	else:
	    authors = obj.authors.all()
	return ','.join([x.get_full_name() for x in authors])