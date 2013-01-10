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
      'current_range': Article.get_current_issue_date_range(),
      'events': Article.events.select_related()[0:10],
      'latest_news_item': Article.news.latest('published_at'),
      'gazette': self.group_for_main(Article.get_current_issue().select_related()[0:5]),
      'columns': Article.columns.select_related()[0:6],
      'videos': Article.objects.filter(type = Article.TYPES.video).select_related()[0:4],
      'blogs': Article.blogs.select_related()[0:5],
      'editorial': Article.editorial.select_related().latest('published_at'),
      'wod': Article.wod.select_related().latest('published_at')
    }
    return context

home = HomeView.as_view()