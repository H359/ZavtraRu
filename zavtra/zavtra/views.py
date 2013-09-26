# -*- coding: utf-8 -*-
import math
from random import choice
from datetime import datetime

from django.views.generic import TemplateView
from django.shortcuts import render

from content.models import Article, Rubric, Issue,\
                           SpecialProject, ExtractedQuote, Panoram
from siteuser.models import User
from zavtra.utils import oneday


class HomeView(TemplateView):
  template_name = 'index.jhtml'

  def get_context_data(self, **kwargs):
    now = datetime.now()
    morning = (now if now.hour >= 7 else now - oneday).replace(hour=7, minute=0, second=0, microsecond=0)
    news_date_range = (morning, morning + oneday)
    selected_articles = Article.columns.defer('content').\
                        prefetch_related('authors').\
                        order_by('-selected_at').\
                        select_related()[0:6]
    blocks = {}
    for b in ['announcements', 'letters', 'izborskyclub']:
      blocks[b] = list(Article.published.filter(rubric__slug = b)[0:1])
    context = {
      'issue_qs': Issue.published.prefetch_related('issue_rubrics'),
      'quotes': ExtractedQuote.objects.all()[0:5],
      'events': Article.events.select_related().defer('content')[0:8],
      'panorams': list(Panoram.objects.all()[0:3]),
      'latest_news': Article.news.defer('content').\
                     filter(published_at__range = news_date_range).\
                     order_by('-selected_at', '-published_at')[0:4],
      'spec_project': SpecialProject.get_current(),
      'selected_articles': selected_articles,
      'video_qs': Article.published.filter(type = Article.TYPES.video),
      'blocks': blocks,
      'blogs': Article.published.prefetch_related('authors').defer('content').\
               filter(selected_at__lte = now, rubric = Rubric.fetch_rubric('blogi'), authors__level__lt = User.USER_LEVELS.system).\
               exclude(pk__in = selected_articles).\
               exclude(rubric__slug__in = blocks.keys()).\
               order_by('-selected_at').
               select_related().all()[0:9],
      'wod_qs': Article.wod.prefetch_related('expert_comments', 'expert_comments__expert').\
                defer('content').select_related()
    }
    return context

home = HomeView.as_view()

def view404(request):
  return render(request, '404.jhtml', {}, content_type='text/html')