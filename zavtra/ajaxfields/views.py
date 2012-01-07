from django.http import HttpResponse
from django.db.models.loading import get_model
from django.db.models import Q
from django.utils import simplejson

def ajaxFieldsResponder(request):
    app = request.GET.get('app')
    obj = request.GET.get('model')
    q = request.GET.get('q')
    model = get_model(app, obj)
    qs = model.objects
    if 'guard' in request.GET:
	guard = request.GET.get('guard').split('|')
	for k in range(0,len(guard)/2):
	    qs = qs.filter(**{guard[k]: guard[k+1]})
    lookup = reduce(lambda a,b: a | b, map(lambda f: Q(**{'%s__startswith' % f : q}), request.GET.get('fields').split(',')))
    # TODO: check for queryset size
    content = simplejson.dumps([(x.pk, unicode(x)) for x in qs.filter(lookup)])
    return HttpResponse(content=content, mimetype='application/json')