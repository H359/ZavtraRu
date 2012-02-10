#-*- coding: utf-8 -*-
import markdown

from functools import wraps

from django.core.cache import cache
from django.conf import settings
from django.http import HttpResponse
from django.template import RequestContext
from django.utils.html import strip_tags

from shortener.models import Link

from pytils.translit import slugify as original_slugify
from mako.lookup import TemplateLookup

class LinkShortenerProcessor(markdown.treeprocessors.Treeprocessor):
    def run(self, tree):
	for x in tree.getiterator('a'):
	    url = x.get('href')
	    if len(url) > 0:
		# get_or_create omits save call? :-/
		try:
		    link = Link.objects.get(original=url)
		except Link.DoesNotExist:
		    link = Link(original=url)
		    link.save()
		x.set("href", link.get_absolute_url())
	return tree

class LinkShortener(markdown.Extension):
    def extendMarkdown(self, md, md_globals):
	md.treeprocessors['shorten'] = LinkShortenerProcessor(md)

class NoStuffMarkdown(markdown.Extension):
    def extendMarkdown(self, md, md_globals):
	del md.inlinePatterns['image_link']
	del md.inlinePatterns['backtick']
	del md.inlinePatterns['reference']
	del md.inlinePatterns['short_reference']
	del md.inlinePatterns['image_reference']
	del md.inlinePatterns['automail']
	del md.inlinePatterns['autolink']
	del md.inlinePatterns['escape']
	del md.parser.blockprocessors['code']
	del md.parser.blockprocessors['hashheader']
	del md.parser.blockprocessors['setextheader']
	del md.parser.blockprocessors['hr']
	print md.inlinePatterns.keys()
	print md.parser.blockprocessors.keys()

class MakoViewMixin(object):
    def render_to_response(self, context, **kwargs):
	return render_to_response(self.template_name, self.request, context)

# TODO: cache this
templateLookup = TemplateLookup(
    directories=settings.TEMPLATE_DIRS,
    output_encoding='utf-8',
    input_encoding='utf-8',
    module_directory=settings.MAKO_MODULE_DIRECTORY
)

def render_to_response(filename, request, context_dict):
    if not isinstance(context_dict, dict):
	return context_dict
    context = RequestContext(request, context_dict)
    data = {}
    [data.update(d) for d in context]
    response = templateLookup.get_template(filename).render_unicode(**data)
    return HttpResponse(content=response, content_type='text/html')

def render_to(filename):
    def decoratee(f):
	@wraps(f)
	def decorator(*args, **kwargs):
	    return render_to_response(filename, args[0], f(*args, **kwargs))
	return decorator
    return decoratee

def slugify(text):
    return original_slugify(strip_tags(text))

def cached(func, key, duration=60*60*24):
    res = cache.get(key)
    if res is None:
        res = func()
        cache.set(key, res, duration)
    return res

def cached_method(key, duration=60*60*24):
    def wrapper(func):
        def method(self, *args, **kwargs):
            return cached(lambda: func(self, *args, **kwargs), key.format(**self.__dict__), duration)
        return method
    return wrapper

def group_list(lst, sz):
    return zip(*[lst[i::sz] for i in range(sz)])