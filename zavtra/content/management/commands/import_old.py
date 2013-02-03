# -*- coding: utf-8 -*-
from datetime import datetime

from django.core.management.base import BaseCommand
from django.db import connection

import old_models as old
from content.models import Rubric, Article, Topic, Issue, RubricInIssue
from siteuser.models import User


class Command(BaseCommand):
  def migrate_rubrics(self):
    self.rubrics = {}
    for rubric in old.Rubric.select():
      self.rubrics[rubric.id] = {
        'rubric': Rubric.objects.create(slug=rubric.slug, title=rubric.title),
        'position': rubric.position
      }
    self.blogs = Rubric.objects.create(title=u'Блоги')

  def migrate_users(self):
    self.users = {}
    for user in old.User.select().where(old.User.is_staff == True):
      if len(user.email) == 0:
        user.email = u'fakeuser_%d@zavtra.ru' % user.id
      self.users[user.id] = User.objects.create(
        email = user.email,
        first_name = user.first_name,
        last_name = user.last_name,
        level = User.USER_LEVELS.staff,
        date_joined = user.date_joined
      )

  def cleanup(self):
    Article.objects.all().delete()
    User.objects.all().delete()
    Rubric.objects.all().delete()
    Issue.objects.all().delete()
    RubricInIssue.objects.all().delete()

  def migrate_article(self, obj):
    if obj.rubric is not None:
      rubric = self.rubrics[obj.rubric.id]['rubric']
    else:
      rubric = self.blogs
    article = Article.objects.create(
      title = obj.title,
      subtitle = obj.subtitle,
      slug = obj.slug,
      status = Article.STATUS.ready,
      type = obj.kind,
      announce = obj.description,
      content = obj.content,
      published_at = obj.pub_date,
      rubric = rubric,
      gazetted = obj.published
    )
    for author in obj.authors:
      article.authors.add(self.users[author.user.id])

  def handle(self, *args, **kwargs):
    old.database.init(args[0], user=args[1])
    self.cleanup()
    self.migrate_rubrics()
    self.migrate_users()
    for obj in old.Article.select().order_by(old.Article.pub_date.desc()):
      self.migrate_article(obj)
    anumber = 149
    rnumber = 41
    ldate = datetime(year=1950, month=1, day=1).date()
    invrubrics = {r['rubric'].id: r['position'] for r in self.rubrics.values()}
    cursor = connection.cursor()
    cursor.execute("""
      SELECT
        DISTINCT date_trunc('day', published_at) AS day
      FROM
        content_article
      WHERE
        gazetted = true
      ORDER BY
        day
    """)
    for d in cursor:
      if ldate != d[0].date():
        articles = Article.objects.filter(
          gazetted = True,
          published_at__year = d[0].year,
          published_at__month = d[0].month,
          published_at__day = d[0].day
        )
        if articles.count() > 0:
          anumber += 1
          if ldate.year != d[0].year:
            rnumber = 1
          else:
            rnumber += 1
          ldate = d[0].date()
          issue = Issue.objects.create(
            absolute_number = anumber,
            relative_number = rnumber,
            published_at = ldate
          )
          rubrics = list(set([article.rubric for article in articles]))
          rubrics.sort(key=lambda r: invrubrics[r.id])
          for pos, rubric in enumerate(rubrics):
            RubricInIssue.objects.get_or_create(issue=issue, rubric=rubric, position=pos)