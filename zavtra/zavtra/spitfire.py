from django.http.response import HttpResponse as Response
from django.template import RequestContext as RC


class SpitfireView(object):
  def render(self, template, context):
    return Response(template(RC(self.request, context)).main())