from django.contrib.sitemaps import Sitemap
from django.db.models import Max
from models import Rubric, FeaturedItems

class RubricsSitemap(Sitemap):
    changefreq = 'monthly'
    priority = 1.0
    
    def items(self):
	return Rubric.objects.annotate(lastmod=Max('contentitem__pub_date')).all()

    def lastmod(self, obj):
	return obj.lastmod

class FeaturedItemsSitemap(Sitemap):
    changefreq = 'weekly'
    priority = 0.5
    
    def items(self):
	return FeaturedItems.objects.annotate(lastmod=Max('tags__taggit_taggeditem_items__contentitem__pub_date')).all()

    def lastmod(self, obj):
	return obj.lastmod