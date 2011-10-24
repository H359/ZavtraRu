import random
import datetime
from threading import Thread

from django.core.management.base import BaseCommand
from django.contrib.auth.models import User
from django.contrib.webdesign.lorem_ipsum import sentence, paragraph, paragraphs, words

from corecontent.models import Article, Issue, IssueTypePage, ArticleOnIssueTypePage

class IssueGenerator(Thread):
    def __init__(self, number, date, users):
        Thread.__init__(self)
        self.number = number
        self.date = date
        self.users = users
    def run(self):
        print self.date
        issue = Issue.objects.create(date=self.date, absolute_number=self.number, relative_number=random.randint(1,30))
        pages = [IssueTypePage.objects.create(issue=issue, title=words(random.randint(1,3), common=False), position = x) for x in range(1,9)]
        for page in pages:
            for position in range(1, random.randint(1,4)):
                article = Article.objects.create(
                    title=words(random.randint(1,5), common=False),
                    subtitle=words(random.randint(1,5), common=False),
                    description=paragraph(),
                    date_pub=self.date,
                    enabled=True,
                    content='\r\n'.join(paragraphs(random.randint(5,15), common=False)))
                for user in random.sample(self.users, random.randint(1,3)):
                    article.authors.add(user)
                ArticleOnIssueTypePage.objects.create(article=article, page=page, position=position)

class Command(BaseCommand):
    absolute_number = 1
    
    def create_users(self):
        users = User.objects.all()
        if users.count() < 5:
            [User.objects.create_user(username=username, email='%s@gmail.com' % username) for username in ['Ivan', 'Petr', 'Sidor', 'Joseph', 'Vladimir']]
        return User.objects.all()
    
    def handle(self, *args, **kwargs):
        self.users = self.create_users()
        stop = datetime.datetime.now().date()
        issue_delta = datetime.timedelta(days=7)
        try:
            lastIssue = Issue.objects.latest('date')
            self.absolute_number = lastIssue.absolute_number + 1
            start = datetime.datetime.combine(lastIssue.date, datetime.time(0,0,0)) + issue_delta
            start = lastIssue.date + issue_delta
        except Exception, e:
            start = datetime.datetime(year=1996,month=10,day=1)
            start = start.replace(day=9-start.weekday()).date()
            print 'ololo', e
        while start < stop:
            generators = []
            for x in range(1, 10):
                gen = IssueGenerator(self.absolute_number, start, self.users)
                self.absolute_number += 1
                start += issue_delta
                gen.start()
                generators.append(gen)
            for x in range(0, len(generators)):
                generators[x].join()
