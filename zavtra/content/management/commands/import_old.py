# -*- coding: utf-8 -*-
import psycopg2
from django.core.management.base import BaseCommand

from content.models import Rubric, Article
from siteuser.models import User


def dictify(desc, data):
  return dict(zip(map(lambda x: x[0], desc), data))


class Command(BaseCommand):
  known_rubrics = {}
  known_authors = {}
  top_rubrics = {}

  def handle(self, *args, **kwargs):
    self.top_rubrics = {
      'gazette': Rubric.objects.create(title=u'Газета'),
      'site': Rubric.objects.create(title=u'Сайт'),
      'blogs': Rubric.objects.create(title=u'Блоги'),
      'word_of_day': Rubric.objects.create(title=u'Слово дня', slug='wod'),
      'editorial': Rubric.objects.create(title=u'Колонка редактора', slug='editorial'),
      'special_project': Rubric.objects.create(title=u'Спецпроект')
    }
    self.conn = psycopg2.connect("dbname=old_zavtra user=root")
    cursor = self.conn.cursor()
    [self.migrate_article(r) for r in self.simple_sql("""
      SELECT c.* FROM corecontent_contentitem AS c
      LEFT JOIN corecontent_rubric AS r
      ON r.id = c.rubric_id
      ORDER BY r.position
    """)]
    cursor.close()
    self.conn.close()

  def simple_sql(self, sql):
    cursor = self.conn.cursor()
    cursor.execute(sql)
    for record in cursor:
      yield dictify(cursor.description, record)
    cursor.close()

  def get_rubric(self, id):
    if id in self.known_rubrics:
      return self.known_rubrics[id]
    r = list(self.simple_sql("SELECT * FROM corecontent_rubric WHERE id=%d" % id))
    published = list(self.simple_sql("SELECT COUNT(id) as cnt FROM corecontent_contentitem WHERE rubric_id=%d AND published=true" % id))[0]
    if published['cnt'] == 0:
      parent = self.top_rubrics['site']
    else:
      parent = self.top_rubrics['gazette']
    self.known_rubrics[id] = Rubric.objects.create(parent=parent, title=r[0]['title'], position=r[0]['position'])
    return self.known_rubrics[id]

  def get_author(self, id):
    if id in self.known_authors:
      return self.known_authors[id]
    a = list(self.simple_sql("SELECT * FROM auth_user WHERE id=%d AND is_staff=True" % id))[0]
    email = a['email']
    if email is None or len(email) == 0:
      email = 'fakeuseremail_%d@zavtra.ru' % id
    self.known_authors[id] = User.objects.create(first_name=a['first_name'], last_name=a['last_name'], level=User.USER_LEVELS.staff, email=email)
    return self.known_authors[id]

  def migrate_article(self, article):
    if article['rubric_id'] is None:
      rubric = self.top_rubrics['blogs']
    else:
      rubric = self.get_rubric(article['rubric_id'])
    created = Article.objects.create(
      title        = article['title'],
      slug         = article['slug'],
      subtitle     = article['subtitle'],
      status       = Article.STATUS.ready,
      type         = article['kind'],
      published_at = article['pub_date'],
      rubric       = rubric,
      announce     = article['description'],
      content      = article['content']
    )
    for a in self.simple_sql("SELECT user_id FROM corecontent_contentitem_authors WHERE contentitem_id=%d" % article['id']):
      created.authors.add(self.get_author(a['user_id']))