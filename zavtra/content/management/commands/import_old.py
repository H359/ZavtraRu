# -*- coding: utf-8 -*-
import psycopg2
from django.core.management.base import BaseCommand

from content.models import Rubric, Article, Topic
from siteuser.models import User


def dictify(desc, data):
  return dict(zip(map(lambda x: x[0], desc), data))


class Command(BaseCommand):
  rubrics = {}
  authors = {}
  topics = {}
  top_rubrics = {}

  def handle(self, *args, **kwargs):
    self.cleanup()
    self.top_rubrics = {      
      'gazette': Rubric.objects.create(title=u'Газета', slug='zeitung'),
      'site': Rubric.objects.create(title=u'Сайт'),
      'blogs': Rubric.objects.create(title=u'Блоги'),
      'word_of_day': Rubric.objects.create(title=u'Слово дня', slug='wod'),
      'editorial': Rubric.objects.create(title=u'Колонка редактора', slug='editorial'),
      'special_project': Rubric.objects.create(title=u'Спецпроект')
    }
    Rubric.objects.create(title=u'Колонки', slug='columns', parent=self.top_rubrics['site'])
    self.conn = psycopg2.connect("dbname=old_zavtra user=root")
    self.migrate_rubrics()
    self.migrate_authors()
    self.migrate_topics()
    self.migrate_articles()
    self.conn.close()

  def simple_sql(self, sql):
    cursor = self.conn.cursor()
    cursor.execute(sql)
    for record in cursor:
      yield dictify(cursor.description, record)
    cursor.close()

  def cleanup(self):
    Rubric.objects.all().delete()
    User.objects.all().delete()
    Article.objects.all().delete()
    Topic.objects.all().delete()

  def migrate_rubrics(self):
    for r in self.simple_sql("""
      SELECT r.*, COUNT(c.id) as is_gazette
      FROM corecontent_rubric AS r
      LEFT JOIN corecontent_contentitem AS c
      ON (c.rubric_id = r.id AND c.published = true)
      GROUP BY r.id
      ORDER BY r.position
    """):
      if r['is_gazette'] > 0:
        parent = self.top_rubrics['gazette']
      elif r['title'] in [u'90-е', u'Врезка']:
        parent = self.top_rubrics['special_project']
      else:
        parent = self.top_rubrics['site']
      self.rubrics[r['id']] = Rubric.objects.create(
        title=r['title'],
        position=r['position'],
        slug=r['slug'],
        parent=parent
      )

  def migrate_authors(self):
    for r in self.simple_sql("SELECT * FROM auth_user WHERE is_staff=true"):
      if r['email'] is None or len(r['email']) == 0:
        email = 'fakeuseremail_%d@zavtra.ru' % r['id']
      else:
        email = r['email']
      self.authors[r['id']] = User.objects.create(
        email=email,
        first_name=r['first_name'],
        last_name=r['last_name'],
        level=User.USER_LEVELS.staff
      )

  def migrate_articles(self):
    article_ctype = list(self.simple_sql("""SELECT id FROM django_content_type WHERE app_label='corecontent' AND model='article'"""))[0]['id']
    for r in self.simple_sql("SELECT * FROM corecontent_contentitem"):
      if r['rubric_id'] is None:
        rubric = self.top_rubrics['blogs']
      else:
        rubric = self.rubrics[r['rubric_id']]
      article = Article.objects.create(
        title        = r['title'],
        slug         = r['slug'],
        subtitle     = r['subtitle'],
        status       = Article.STATUS.ready,
        type         = r['kind'],
        published_at = r['pub_date'],
        rubric       = rubric,
        announce     = r['description'], # clean html, extract cover
        content      = r['content'] # clean html
      )
      for a in self.simple_sql("SELECT user_id FROM corecontent_contentitem_authors WHERE contentitem_id=%d" % r['id']):
        article.authors.add(self.authors[a['user_id']])
      for t in self.simple_sql("""
        SELECT
          ft.featureditems_id AS id
        FROM
          corecontent_featureditems_tags AS ft
        WHERE
          ft.tag_id IN (
            SELECT
              tti.tag_id
            FROM
              taggit_taggeditem AS tti
            WHERE
              tti.content_type_id = %d
              AND
              tti.object_id = %d
          )
      """ % (article_ctype, r['id'])):
        article.topics.add(self.topics[t['id']])

  def migrate_topics(self):
    for r in self.simple_sql("SELECT * FROM corecontent_featureditems"):
      self.topics[r['id']] = Topic.objects.create(title=r['title'], slug=r['slug'], on_top=r['is_active'])