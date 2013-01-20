# -*- coding: utf-8 -*-
import math
from django.views.generic import TemplateView

from content.models import Article, Rubric, Issue
from siteuser.models import User

def group_by(coll, sep):
  steps = int(math.ceil(len(coll) / (1.0*sep)))
  return [coll[sep*p:sep*(p+1)] for p in range(0, steps)]


class HomeView(TemplateView):
  template_name = 'index.jhtml'

  def get_context_data(self, **kwargs):
    print Article.events.count()
    context = {
      'issue': Issue.published.latest('published_at'),
      'events': Article.events.all(),
      'latest_news_item': Article.news.latest('published_at'),
      #'gazette': Article.get_current_issue().select_related().all()[0:5],
      #'columns': group_by(Article.columns.select_related()[0:6], 3),
      #'videos': Article.objects.filter(type = Article.TYPES.video).select_related()[0:4],
      #'blogs': Article.blogs.select_related()[0:5],
      #'editorial': Article.editorial.select_related().latest('published_at'),
      'wod': Article.wod.select_related().latest('published_at')
    }
    return context

home = HomeView.as_view()