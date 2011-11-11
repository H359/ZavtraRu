"""Sitemaps for minipoll"""
from django.contrib.sitemaps import Sitemap

from minipoll.models import Poll

class PollSitemap(Sitemap):
    """Sitemap for minipoll.poll"""
    priority = 0.5

    def items(self):
        return Poll.published.all()
    
    def lastmod(self, obj):
        return obj.creation_date
