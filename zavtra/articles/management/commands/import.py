import psycopg2
from datetime import datetime

from django.core.management.base import BaseCommand

from users.models import User
from articles.models import Rubric, Article, ArticlePart, HTMLText

class Command(BaseCommand):
    now = datetime.now()
    def describe(self, desc, row):
        res = {}
        for n, x in enumerate(desc):
            res[x[0]] = row[n]
        return res
        
    def migrate_users(self):
        self.cursor.execute("SELECT * FROM auth_user")
        desc = self.cursor.description
        while True:
            p = self.cursor.fetchone()
            if p is None:
                break
            pd = self.describe(desc, p)
            pd['dob'] = self.now
            pd['gender'] = 1
            User.objects.create(**pd)
    
    def migrate_rubrics(self):
        self.cursor.execute("SELECT title,slug FROM corecontent_rubric")
        desc = self.cursor.description
        while True:
            p = self.cursor.fetchone()
            if p is None:
                break
            pd = self.describe(desc, p)
            Rubric.objects.create(**pd)

    #def migrate_tags(self):
    #    pass
    
    def migrate_articles(self):
        self.cursor.execute("SELECT * FROM corecontent_contentitem")
        desc = self.cursor.description
        while True:
            p = self.cursor.fetchone()
            if p is None:
                break
            pd = self.describe(desc, p)
            h = HTMLText.objects.create(text=pd['content'])
            article = Article.objects.create(
                title=pd['title'],
                sub_title=pd.get('sub_title', ''),
                pub_date=pd['pub_date'],
                published=pd['published'],
                enabled=pd['enabled'],
                exclusive=pd['exclusive'],
                rubric=Rubric.objects.get(pk=pd['rubric_id']),
            )
            part = ArticlePart(article=article, content_object=h)
            part.save()
        
    def handle(self, *args, **kwargs):
        self.connection = psycopg2.connect("dbname=zavtra user=zavtra")
        self.cursor = self.connection.cursor()
        try:
            #print 'Migrating users'
            #self.migrate_users()
            #print 'Migrating rubrics'
            #self.migrate_rubrics()
            #self.migrate_tags()
            print 'Migrating articles'
            self.migrate_articles()
        except Exception, e:
            print e
        self.cursor.close()
        self.connection.close()