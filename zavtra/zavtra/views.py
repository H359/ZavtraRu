# -*- coding: utf-8 -*-
import math
from random import choice
from datetime import datetime

from django.views.generic import TemplateView

from content.models import Article, Rubric, Issue
from siteuser.models import User


"""
def group_by(coll, sep):
  steps = int(math.ceil(len(coll) / (1.0*sep)))
  return [coll[sep*p:sep*(p+1)] for p in range(0, steps)]
"""


class HomeView(TemplateView):
  template_name = 'index.jhtml'

  def get_context_data(self, **kwargs):
    now = datetime.now()
    selected_articles = Article.columns.defer('content').\
                        prefetch_related('authors').\
                        order_by('-selected_at').\
                        select_related()[0:6]
    context = {
      'issue': Issue.published.prefetch_related('issue_rubrics').latest('published_at'),
      'events': Article.events.select_related().defer('content').all(),
      'latest_news': Article.news.defer('content').all()[0:3],
      'selected_articles': selected_articles,
      'videos': list(Article.published.filter(type = Article.TYPES.video).defer('content')[0:4]),
      'blogs': Article.published.prefetch_related('authors').defer('content').\
               filter(selected_at__lte = now).exclude(pk__in = selected_articles).\
               select_related().all()[0:6],
      'wod': Article.wod.prefetch_related('expert_comments', 'expert_comments__expert').\
             defer('content').select_related().latest('published_at')
    }
    return context

home = HomeView.as_view()