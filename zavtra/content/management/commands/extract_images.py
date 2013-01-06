from pyquery import PyQuery as pq
from lxml import etree, html
import urllib
import os
from django.core.management.base import BaseCommand
from django.core.files import File

from content.models import Article


class Command(BaseCommand):
  def handle(self, *args, **kwargs):
    [self.process(article) for article in Article.everything.filter(cover_source = '')[0:30]]

  def fetch_img(self, article, img):
    fullpath = 'http://zavtra.ru%s' % img
    result = urllib.urlretrieve(fullpath)
    article.cover_source.save(os.path.basename(img), File(open(result[0], 'rb')))
    article.save()

  def process(self, article):
    try:
      d = pq(html.fromstring(article.announce))
      for img in d('img:first'):
        self.fetch_img(article, img.get('src'))
    except etree.XMLSyntaxError, e:
      pass