# -*- coding: utf-8 -*-
from django.http import HttpResponse, Http404

from ajax_filtered_fields import utils

def json_index(request):
    """
    Answer to ajax requests of the client returning
    JSON serialized data.
    """
    # give a response only if this is an ajax request
    if request.is_ajax():
        # get application label and object name
        app_label = request.GET.get('app_label')
        object_name = request.GET.get('object_name')
        
        if app_label and object_name:
            from django.db.models.loading import get_model
            # get the model
            model = get_model(app_label, object_name)
            
            if model is not None:
                from django.utils import simplejson
                # get the lookup dict
                lookup_string = request.GET.get('lookup_string', '')
                lookup_dict = utils.stringToLookup(lookup_string)
                # get the select related part
                select_related = utils._cleanValue(
                    request.GET.get('select_related'))
                # get the queryset
                objects = utils.getObjects(model, lookup_dict, select_related)
                # get the raw data and dump the json
                raw_data = [(i.pk, unicode(i)) for i in objects]
                data = simplejson.dumps(raw_data)
                # return data with the right content type
                return HttpResponse(data, content_type="application/json")
                
    raise Http404
