from utils.view_utils import render_to

from django.contrib.auth.models import User
from articles.models import Rubric

@render_to('home.html')
def home(request):
    return {
	'rubrics': Rubric.objects.all()[0:7],
	'columnists': User.objects.all()[0:5]
    }