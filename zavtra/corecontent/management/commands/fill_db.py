#-*- coding: utf-8 -*-
import random
import datetime
from threading import Thread

from django.core.management.base import BaseCommand
from django.contrib.auth.models import User
from django.contrib.webdesign.lorem_ipsum import sentence, paragraph, paragraphs, words

from taggit.models import Tag

from corecontent.models import Article, Issue, IssueTypePage, ArticleOnIssueTypePage, Rubric

class IssueGenerator(Thread):
    def __init__(self, number, date, users, rubrics, tags):
        Thread.__init__(self)
        self.number = number
        self.date = date
        self.users = users
        self.rubrics = rubrics
        self.tags = tags
    def create_article(self, date):
        article = Article.objects.create(
            title=words(random.randint(1,5), common=False).capitalize(),
            subtitle=words(random.randint(1,5), common=False).capitalize(),
            description=paragraph(),
            date_pub=date,
            enabled=True,
            content='\r\n'.join(paragraphs(random.randint(5,15), common=False)))
        if random.uniform(0,1) > 0.7:
            article.rubric = random.choice(self.rubrics)
            article.save()
        return article
    def run(self):
        print self.date
        issue = Issue.objects.create(date=self.date, absolute_number=self.number, relative_number=random.randint(1,30))
        pages = [IssueTypePage.objects.create(issue=issue, title=words(random.randint(1,3), common=False), position = x) for x in range(1,9)]
        for page in pages:
            for position in range(1, random.randint(1,4)):
                article = self.create_article(self.date)
                for user in random.sample(self.users, random.randint(1,3)):
                    article.authors.add(user)
                article.contentitem_ptr.tags.add(*(random.sample(self.tags, random.randint(1,5))))
                ArticleOnIssueTypePage.objects.create(article=article, page=page, position=position)
        for x in range(0,random.randint(0,10)):
            article = self.create_article(self.date)
            for user in random.sample(self.users, random.randint(1,3)):
                article.authors.add(user)
            article.contentitem_ptr.tags.add(*(random.sample(self.tags, random.randint(1,5))))

class Command(BaseCommand):
    absolute_number = 1
    
    def create_rubrics(self):
        rubrics = [u'Политика', u'Выборы', u'Аналитика', u'Интервью', u'События', u'Культура']
        self.rubrics = map(lambda w: Rubric.objects.create(title=w, on_main=True), rubrics)
    
    def create_tags(self):
        tags = [u'Каддафи', u'Россия', u'Кавказ', u'Путин', u'Кудрин', u'Медведев', u'История', u'Власть', u'Армия', u'Закон', u'Террор']
        self.tags = map(lambda w: Tag.objects.create(name=w), tags)

    def create_users(self):
        users = User.objects.all()
        if users.count() < 5:
            [User.objects.create_user(username=username, email='%s@gmail.com' % username) for username in ['Ivan', 'Petr', 'Sidor', 'Joseph', 'Vladimir']]
        self.users = User.objects.all()
    
    def handle(self, *args, **kwargs):
        self.create_users()
        self.create_rubrics()
        self.create_tags()
        stop = datetime.datetime.now().date()
        issue_delta = datetime.timedelta(days=7)
        try:
            lastIssue = Issue.objects.latest('date')
            self.absolute_number = lastIssue.absolute_number + 1
            start = datetime.datetime.combine(lastIssue.date, datetime.time(0,0,0)) + issue_delta
            start = lastIssue.date + issue_delta
        except Exception, e:
            start = datetime.datetime(year=2011,month=1,day=1)
            start = start.replace(day=9-start.weekday()).date()
            print 'ololo', e
        done = False
        while not done:
            generators = []
            for x in range(1, 20):
                gen = IssueGenerator(self.absolute_number, start, self.users, self.rubrics, self.tags)
                self.absolute_number += 1
                start += issue_delta
                gen.start()
                generators.append(gen)
                if start >= stop:
                    done = True
                    break
            for x in range(0, len(generators)):
                generators[x].join()
