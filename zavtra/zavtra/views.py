from utils.view_utils import render_to

from django.contrib.auth.models import User
from articles.models import Article, Rubric

@render_to('home.html')
def home(request):
    return {
        'rubrics': Rubric.objects.all()[0:6],
        'columnists': User.objects.filter(first_name__isnull=False, last_name__isnull=False)[0:5],
        'articles': Article.archive.all()[0:10]
    }