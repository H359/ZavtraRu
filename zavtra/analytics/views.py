#-*- coding: utf-8 -*-
from datetime import datetime, timedelta
from psycopg2.extras import DictCursor
from django.views.generic import TemplateView
from django.db import connection, transaction
from django.contrib.contenttypes.models import ContentType
from analytics.models import Hit


class HitsView(TemplateView):
    template_name = 'analytics/hits.jhtml'

    def __init__(self):
        now = datetime.now()
        fmt = '%Y-%m-%d'
        self.quants = [(u'месяцу', 'month'), (u'неделе', 'week'), (u'дню', 'day')]
        self.types = list(ContentType.objects.filter(pk__in = Hit.objects.distinct().values_list('content_type', flat=True)))
        self.default_data = {
            'quant': 0,
            'start': (now-timedelta(days=1)).strftime(fmt),
            'end': now.strftime(fmt),
            'type': self.types[0].id
        }

    def quantizer(self, q):
        return 'date_trunc(\'%s\', "datetime") AS dt' % self.quants[q][1]

    def get_rows(self, data):
        results = {}
        cursor = connection.connection.cursor(name='stat_cursor', cursor_factory=DictCursor)
        cursor.execute("""
          SELECT object_id, Sum(1) AS hits
          FROM analytics_hit
          WHERE content_type_id = %s AND datetime >= %s AND datetime < %s
          GROUP BY object_id
          ORDER BY hits DESC
          LIMIT 20
        """, (data['type'], data['start'], data['end']))
        for x in cursor:
          results[x['object_id']] = dict(hits=x['hits'], object=None, d={})
        cursor.close()
        return results

    def populate_rows(self, data, rows):
        intervals = set()
        typ_id    = data['type']
        model     = self.types[ (i for i, v in enumerate(self.types) if v.id == typ_id).next() ].model_class()
        for o in model.objects.filter(pk__in = rows.keys()):
          rows[o.id]['object'] = o
        cursor = connection.connection.cursor(name='stat_cursor', cursor_factory=DictCursor)
        cursor.execute("SELECT COUNT(id) AS hits, object_id, " + self.quantizer(data['quant']) + " FROM analytics_hit " +\
                       "WHERE object_id IN (" + ','.join(map(str, rows.keys())) + ") " +\
                       "GROUP BY object_id, dt " +\
                       "ORDER BY dt")
        for x in cursor:
          intervals.add(x['dt'])
          rows[x['object_id']]['d'][x['dt']] = x['hits']
        cursor.close()
        return rows, intervals

    def get_context_data(self, **kwargs):
        data = dict(self.default_data, **{k: v for k, v in self.request.GET.iteritems() if len(v) > 0})
        data['type'] = int(data['type'])
        data['quant'] = int(data['quant'])
        with transaction.atomic():
          rows, intervals = self.populate_rows(data, self.get_rows(data))
        context = {
          'quants': enumerate([x[0] for x in self.quants]),
          'types': self.types,
          'intervals': sorted(list(intervals)),
          'rows': sorted(rows.values(), key=lambda x: x['hits'])
        }
        return dict(data, **context)
