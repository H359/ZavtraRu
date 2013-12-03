#-*- coding: utf-8 -*-
from django.contrib.syndication.views import Feed
from django.utils import feedgenerator
from django.utils.html import remove_tags
from content.models import Article


class YaFeed(feedgenerator.Rss201rev2Feed):
  # def add_item(self, *args, **kwargs):
  #   print kwargs
  #   kwargs['item_content'] = None
  #   super(YaFeed, self).add_item(*args, **kwargs)

  def root_attributes(self):
    attrs = super(YaFeed, self).root_attributes()
    attrs['xmlns:yandex'] = 'http://news.yandex.ru'
    attrs['xmlns:media'] = 'http://search.yahoo.com/mrss/'
    return attrs

  def add_root_elements(self, handler):
    super(YaFeed, self).add_root_elements(handler)
    handler.startElement('image', {})
    handler.addQuickElement('url', 'http://zavtra.ru/static/img/logo_mini.jpg')
    handler.addQuickElement('title', u'Газета "Завтра"')
    handler.addQuickElement('link', 'http://zavtra.ru"')
    handler.endElement('image')

  def add_item_elements(self, handler, item):
    super(YaFeed, self).add_item_elements(handler, item)
    handler.addQuickElement('yandex:full-text', item['content'])


class EventsFeed(Feed):
  feed_type = YaFeed
  title = u'События и новости'
  link = 'http://zavtra.ru/'
  description = u'Газета "Завтра"'

  TAGS = """
  p div span b strong iframe em i a img table tr td thead tbody tfoot th br hr
  """

  def items(self):
    return Article.common_news.select_related()[0:15]

  def item_title(self, item):
    return item.title

  def item_description(self, item):
    return item.announce

  def item_pubdate(self, item):
    return item.published_at

  def item_extra_kwargs(self, item):
    return {
      'content': remove_tags(item.content, self.TAGS)
    }