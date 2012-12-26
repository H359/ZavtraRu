# -*- coding: utf-8 -*-
from django.views.generic import TemplateView

from content.models import Article, Rubric
from siteuser.models import User


class HomeView(TemplateView):
  template_name = 'index.jhtml'

  def group_for_main(self, articles):
    groups = {u'Также в номере': []}
    for num, article in enumerate(articles):
      if num == 0:
        groups[article.rubric.title] = [article]
      else:
        groups[u'Также в номере'].append(article)
    return groups

  def get_context_data(self, **kwargs):
    context = {
      'current_number': Article.get_current_issue_number(),
      'events': Article.news.select_related()[0:10],
      'gazette': self.group_for_main(Article.get_current_issue().select_related()[0:5]),
      'columns': Article.columns.select_related()[0:6],
      'blogs': Article.blogs.select_related()[0:5],
      'wod': Article.wod.latest('published_at')
    }
    return context

home = HomeView.as_view()