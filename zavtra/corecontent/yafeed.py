#-*- coding: utf-8 -*-
from django.utils.xmlutils import SimplerXMLGenerator
from django.utils.feedgenerator import Rss201rev2Feed

class YXG(SimplerXMLGenerator):
    def addQuickElementCDATA(self, name, contents=None, attrs=None):
	if attrs is None: attrs = {}
	self.startElement(name, attrs)
	if contents is not None:
	    self._write('<![CDATA[%s]]>' % contents)
	self.endElement(name)

class Rss(Rss201rev2Feed):
    def write(self, outfile, encoding):
	handler = YXG(outfile, encoding)
	handler.startDocument()
	handler.startElement(u'rss', self.rss_attributes())
	handler.startElement(u'channel', self.root_attributes())
	self.add_root_elements(handler)
	self.write_items(handler)
	self.endChannelElement(handler)
	handler.endElement(u'rss')

    def add_item_elements(self, handler, item):
	desc = item.get('description')
	if desc is not None:
	    handler.addQuickElement(u'description', desc)
	    item['description'] = None
	super(Rss, self).add_item_elements(handler, item)

class YandexNewsRss(Rss):
    def rss_attributes(self):
	attrs = super(YandexNewsRss, self).rss_attributes()
	del attrs['xmlns:atom']
	attrs.update({'xmlns:yandex': 'http://news.yandex.ru', 'xmlns:media': 'http://search.yahoo.com/mrss/'})
	return attrs

    def add_root_elements(self, handler):
	#super(YandexNewsRss, self).add_root_elements(handler)
	for i in [u'title', u'link', u'description']:
	    handler.startElement(i, {})
	    handler.characters(self.feed[i])
	    handler.endElement(i)
	handler.startElement(u'image', {})
	handler.addQuickElement(u'url', u'http://zavtra.ru/static/img/logo.gif')
	handler.addQuickElement(u'title', u'Газета Завтра')
	handler.addQuickElement(u'link', u'http://zavtra.ru')
	handler.endElement(u'image')

    def add_item_elements(self, handler, item):
	ftext = item.get('fulltext')
	pdate = item.get('pubDate')
	if ftext is not None:
	    handler.addQuickElement(u'yandex:full-text', item['fulltext'])
	if pdate is not None:
	    handler.addQuickElement(u'pubDate', pdate)
	super(YandexNewsRss, self).add_item_elements(handler, item)