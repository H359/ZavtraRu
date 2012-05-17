import psycopg2

from django.core.management.base import BaseCommand

from users.models import User
from articles.models import Rubric, Article, ArticlePart, HTMLText

class Command(BaseCommand):
    def describe(self, desc, row):
        res = {}
        for n, x in enumerate(desc):
            res[x[0]] = row[n]
        return res
        
    def migrate_users(self):
        self.cursor.execute("SELECT * FROM auth_user")
        desc = self.cursor.description
        while (p = cur.fetchone()) is not None:
            pd = self.describe(desc, p)
            User.objects.create(**pd)
    
    def migrate_rubrics(self):
        self.cursor.execute("SELECT title,slug FROM corecontent_rubric")
        desc = self.cursor.description
        while (p = cur.fetchone()) is not None:
            pd = self.describe(desc, p)
            Rubric.objects.create(**pd)

    #def migrate_tags(self):
    #    pass
    
    def migrate_articles(self):
        self.cursor.execute("SELECT * FROM corecontent_contentitem")
        desc = self.cursor.description
        while (p = cur.fetchone()) is not None:
            pd = self.describe(desc, p)
            h = HTMLText.objects.create(text=pd['content'])
            article = Article.objects.create(
                title=pd['title'],
                sub_title=pd['sub_title'],
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
            self.migrate_users()
            self.migrate_rubrics()
            #self.migrate_tags()
            self.migrate_articles()
        except Exception, e:
            print e
        self.cursor.close()
        self.connection.close()