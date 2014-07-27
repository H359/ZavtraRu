#-*- coding: utf-8 -*-
from datetime import datetime, timedelta
from django.views.generic import TemplateView
from django.contrib.contenttypes.models import ContentType
from analytics.models import Hit


class HitsView(TemplateView):
    template_name = 'analytics/hits.jhtml'

    def __init__(self):
        self.quants = enumerate([u'месяцу', u'неделе', u'дню', u'часу'])
        self.default_data = {'quant': 0, 'start': '', 'end': ''}

    def quantize(self, qs, quant):
        return qs

    def get_context_data(self, **kwargs):
        data = dict(self.default_data, **{k: v for k, v in self.request.GET.iteritems() if len(v) > 0})
        print data
        qs = self.quantize(Hit.objects.all(), data['quant'])
        if 'type' in data:
            qs = qs.filter(content_type__id = data['type'])
        context = {
            'quants': self.quants,
            'types': ContentType.objects.filter(pk__in = Hit.objects.distinct().values_list('content_type', flat=True)),
            'data': qs
        }
        return dict(data, **context)
