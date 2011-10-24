import random
import datetime

from django.core.management.base import BaseCommand
from django.contrib.auth.models import User
from django.contrib.webdesign.lorem_ipsum import sentence, paragraph, paragraphs, words

from corecontent.models import Article, Issue, IssueTypePage, ArticleOnIssueTypePage

class Command(BaseCommand):
    absolute_number = 1
    def generate_issue(self, date):
        print date
        issue = Issue.objects.create(date=date, absolute_number=self.absolute_number, relative_number=1)
        self.absolute_number += 1
        pages = [IssueTypePage.objects.create(issue=issue, title=words(random.randint(1,3)), position = x) for x in range(1,9)]
        for page in pages:
            for position in range(1, random.randint(2,5)):
                article = Article.objects.create(
                    title=words(random.randint(1,5)),
                    subtitle=words(random.randint(1,5)),
                    description=paragraph(),
                    date_pub=date,
                    enabled=True,
                    content='\r\n'.join(paragraphs(random.randint(5,15))))
                for user in random.sample(self.users, random.randint(1,3)):
                    article.authors.add(user)
                ArticleOnIssueTypePage.objects.create(article=article, page=page, position=position)
    
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
            #start = datetime.datetime.combine(lastIssue.date, datetime.time(0,0,0)) + issue_delta
            start = lastIssue.date + issue_delta
        except Exception, e:
            start = datetime.datetime(year=2010,month=1,day=1)
            start = start.replace(day=9-start.weekday()).date()
            print 'ololo', e
        while start < stop:
            self.generate_issue(start)
            start += issue_delta
