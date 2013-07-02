# -*- coding: utf-8 -*-
import math
from random import choice
from datetime import datetime

from django.views.generic import TemplateView
from django.shortcuts import render

from content.models import Article, Rubric, Issue, DailyQuote, SpecialProject
from siteuser.models import User
from zavtra.utils import oneday


class HomeView(TemplateView):
  template_name = 'index.jhtml'

  def get_context_data(self, **kwargs):
    now = datetime.now()
    morning = (now if now.hour >= 7 else now - oneday).replace(hour=7, minute=0, second=0, microsecond=0)
    news_date_range = (morning, morning + oneday)
    #print morning, morning + oneday
    selected_articles = Article.columns.defer('content').\
                        prefetch_related('authors').\
                        exclude(authors__level = User.USER_LEVELS.system).\
                        order_by('-selected_at').\
                        select_related()[0:6]
    try:
      editorial = Article.editorial.select_related().\
                  prefetch_related('authors').\
                  latest('published_at')
    except Article.DoesNotExist:
      editorial = None
    try:
      spec_project = SpecialProject.objects.prefetch_related('articles').\
                     filter(date__lte = now.date()).latest('date')
    except SpecialProject.DoesNotExist:
      spec_project = None
    context = {
      'issue_qs': Issue.published.prefetch_related('issue_rubrics'),
      'editorial': editorial,
      'quote': DailyQuote.get_current(),
      'events': Article.events.select_related().defer('content')[0:8],
      'latest_news': Article.news.defer('content').\
                     filter(published_at__range = news_date_range).\
                     order_by('-selected_at', '-published_at')[0:4],
      'spec_project': spec_project,
      'selected_articles': selected_articles,
      'video_qs': Article.published.filter(type = Article.TYPES.video),
      'blogs': Article.published.prefetch_related('authors').defer('content').\
               filter(selected_at__lte = now, rubric = Rubric.fetch_rubric('blogi'), authors__level__lt = User.USER_LEVELS.system).\
               exclude(pk__in = selected_articles).\
               order_by('-selected_at').
               select_related().all()[0:6],
      'wod_qs': Article.wod.prefetch_related('expert_comments', 'expert_comments__expert').\
             defer('content').select_related(),
      'system_blogs': Article.published.prefetch_related('authors').defer('content').\
		      filter(selected_at__lte = now, rubric = Rubric.fetch_rubric('blogi'), authors__level = User.USER_LEVELS.system).\
		      exclude(pk__in = selected_articles).\
		      order_by('-selected_at').\
		      select_related().all()[0:6],
    }
    return context

home = HomeView.as_view()

def view404(request):
  return render(request, '404.jhtml', {}, content_type='text/html')