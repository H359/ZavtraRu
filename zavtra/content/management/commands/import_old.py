# -*- coding: utf-8 -*-
import psycopg2
from django.core.management.base import BaseCommand

from content.models import Rubric, Article, Topic, Issue, RubricInIssue
from siteuser.models import User


def dictify(desc, data):
  return dict(zip(map(lambda x: x[0], desc), data))


class Command(BaseCommand):
  authors = {}
  topics = {}

  def handle(self, *args, **kwargs):
    self.cleanup()
    self.conn = psycopg2.connect("dbname=%s user=%s" % args)
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
    Issue.objects.all().delete()
    RubricInIssue.objects.all().delete()
    Rubric.objects.all().delete()
    User.objects.all().delete()
    Article.objects.all().delete()
    Topic.objects.all().delete()

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
    """
    for r in self.simple_sql("SELECT * FROM corecontent_rubric ORDER BY position"):
      self.rubrics[r['id']] = Rubric.objects.create(title=r['title'], slug=r['slug'])
    self.blogs = Rubric.objects.create(title=u'Блоги')
    self.events = Rubric.objects.create(title=u'События')
    self.news = Rubric.objects.create(title=u'Новости')
    self.word_of_day = Rubric.objects.create(title=u'Слово дня')
    self.editorial = Rubric.objects.create(title=u'Колонка редактора')
    self.special_project = Rubric.objects.create(title=u'Спецпроект')
    """
    # zeitung
    rubrics = {}
    for r in self.simple_sql("SELECT * FROM corecontent_rubric"):
      rubrics[r['id']] = (Rubric.objects.create(title=r['title'], slug=r['slug']), r['position'])

    absnumber = 150
    relnumber = 1
    for issue_date in self.simple_sql("SELECT DISTINCT date_trunc('day', pub_date) AS dt FROM corecontent_contentitem WHERE published=true ORDER BY dt ASC"):
      issue = Issue.objects.create(absolute_number=absnumber, relative_number=relnumber, published_at=issue_date['dt'].date())
      # illustration?
      ir = {}
      for r in self.simple_sql("SELECT * FROM corecontent_contentitem WHERE date_trunc('day', pub_date) = '%s' AND published=true" % issue_date['dt']):
        if r['rubric_id'] not in ir:
          rub, pos = rubrics[r['rubric_id']]
          ir[r['rubric_id']] = RubricInIssue.objects.create(issue=issue, rubric=rub, position=pos)
        else:
          rub, _ = rubrics[r['rubric_id']]
        article = Article.objects.create(
          title        = r['title'],
          slug         = r['slug'],
          subtitle     = r['subtitle'],
          status       = Article.STATUS.ready,
          type         = r['kind'],
          published_at = issue_date['dt'],
          rubric       = rub,
          selected_at  = None,
          announce     = r['description'], # clean html, extract cover
          content      = r['content'] # clean html
        )        
      absnumber += 1
      relnumber += 1
      for a in self.simple_sql("SELECT user_id FROM corecontent_contentitem_authors WHERE contentitem_id=%d" % r['id']):
        article.authors.add(self.authors[a['user_id']])

    blogs = Rubric.objects.create(title=u'Блоги')
    # other stuff
    for r in self.simple_sql("SELECT * FROM corecontent_contentitem WHERE published=false"):
      if r['rubric_id'] is not None:
        rub, _ = rubrics[r['rubric_id']]
      else:
        rub = blogs
      article = Article.objects.create(
        title        = r['title'],
        slug         = r['slug'],
        subtitle     = r['subtitle'],
        status       = Article.STATUS.ready,
        type         = r['kind'],
        published_at = r['pub_date'],
        rubric       = rub,
        selected_at  = None,
        announce     = r['description'], # clean html, extract cover
        content      = r['content'] # clean html
      )        
      for a in self.simple_sql("SELECT user_id FROM corecontent_contentitem_authors WHERE contentitem_id=%d" % r['id']):
        article.authors.add(self.authors[a['user_id']])

  def migrate_topics(self):
    for r in self.simple_sql("SELECT * FROM corecontent_featureditems"):
      self.topics[r['id']] = Topic.objects.create(title=r['title'], slug=r['slug'], on_top=r['is_active'])