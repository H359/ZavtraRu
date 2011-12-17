from datetime import datetime

from haystack import indexes
from models import ContentItem

class ContentItemIndex(indexes.SearchIndex, indexes.Indexable):
    KIND_MAP = {'text': 1, 'video': 2, 'image': 3}
    title   = indexes.CharField(boost=1.5)
    rubric  = indexes.CharField()
    authors = indexes.FacetMultiValueField(boost=1.25)
    kind    = indexes.IntegerField()
    text    = indexes.CharField(document=True, model_attr='content')

    def prepare_kind(self, obj):
	return self.KIND_MAP.get(obj.kind, 0)

    def get_model(self):
	return ContentItem

    def index_queryset(self):
	now = datetime.now()
	return self.get_model().objects.filter(enabled=True, pub_date__lte = now)