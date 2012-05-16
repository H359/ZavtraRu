#-*- coding: utf-8 -*-
from functools import wraps

from django.conf import settings
from django.http import HttpResponse
from django.template import RequestContext

from mako.lookup import TemplateLookup

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