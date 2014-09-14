#-*- coding: utf-8 -*-
import csv
from datetime import datetime, timedelta
from django.http import StreamingHttpResponse
from django.contrib.contenttypes.models import ContentType
from django.db import connection, transaction
from psycopg2.extras import DictCursor

from content.models import Article, Topic


article_ctype = ContentType.objects.get_for_model(Article)

class Echo(object):
  def write(self, value):
    return value

def csv_view_data(year, month):
  writer = csv.writer(Echo())
  with transaction.atomic():
    cursor = connection.connection.cursor(name='stat_cursor', cursor_factory=DictCursor)
    cursor.execute("""
      SELECT
        h.object_id, a.title,
        COUNT(h.id) AS hits FROM analytics_hit AS h
      INNER JOIN content_article AS a ON a.id=h.object_id
      GROUP BY h.object_id, a.title
      ORDER BY hits
    """)
    for x in cursor:
      ts = u','.join([t.title for t in Topic.objects.filter(articles__in = [x['object_id']])])
      yield writer.writerow((x['title'].encode('utf-8'), x['hits'], ts.encode('utf-8')))

def csv_view(request, year, month):
  return StreamingHttpResponse(csv_view_data(year, month), content_type='text/csv')
