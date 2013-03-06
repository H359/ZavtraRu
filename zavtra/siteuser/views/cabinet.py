#-*- coding: utf-8 -*-
from django.views.generic import TemplateView, ListView
from django.views.generic.edit import FormView
from django.core.urlresolvers import reverse
from django.contrib import messages
from django.shortcuts import redirect
from django.db.models import Q

from base import FilteredArticlesView
from siteuser.forms import UserInfoForm, ArticleForm
from siteuser.models import User
from content.models import Rubric


class CabinetView(TemplateView, FormView):
  template_name = 'siteuser/cabinet_about.jhtml'

  def get_context_data(self, **kwargs):
    context = super(CabinetView, self).get_context_data(**kwargs)
    context['part'] = 'about'
    return context

  def return_form(self, form):
    return self.render_to_response(self.get_context_data(form=form))

  def post(self, request, *args, **kwargs):
    form = UserInfoForm(request.POST, instance=request.user)
    if form.is_valid():
      if request.user.level > 0:
        messages.info(request, u'Запрос на обновление данных отправлен редактору')
      else:
        form.save()
      return redirect('siteuser.view.cabinet')
    return self.return_form(form)

  def get(self, request, *args, **kwargs):
    return self.return_form(UserInfoForm(instance=request.user))


class CabinetArticlesView(FilteredArticlesView):
  template_name = 'siteuser/cabinet_articles.jhtml'

  def get_categories(self):
    return (
      (u'все', None, reverse('siteuser.view.cabinet_articles')),
      (u'статьи', 'text', reverse('siteuser.view.cabinet_articles_category', kwargs={'category': 'text'})),
      (u'видео', 'video', reverse('siteuser.view.cabinet_articles_category', kwargs={'category': 'video'})),
      (u'слова дня', 'wod', reverse('siteuser.view.cabinet_articles_category', kwargs={'category': 'wod'}))
    )

  def get_queryset(self):
    qs = super(CabinetArticlesView, self).get_queryset()
    return qs.filter(
      Q(authors__in = [self.request.user]) | Q(expert_comments__expert = self.request.user)
    )


class CabinetArticlesSubscriptionsView(FilteredArticlesView):
  template_name = 'siteuser/cabinet_subscriptions_articles.jhtml'

  def get_context_data(self, **kwargs):
    context = super(CabinetArticlesSubscriptionsView, self).get_context_data(**kwargs)
    context['part'] = 'subscriptions'
    return context

  def get_categories(self):
    return (
      (u'все', None, reverse('siteuser.view.cabinet_subscriptions_articles')),
      (u'статьи', 'text', reverse('siteuser.view.cabinet_subscriptions_articles_category', kwargs={'category': 'text'})),
      (u'видео', 'video', reverse('siteuser.view.cabinet_subscriptions_articles_category', kwargs={'category': 'video'})),
      (u'слова дня', 'wod', reverse('siteuser.view.cabinet_subscriptions_articles_category', kwargs={'category': 'wod'}))
    )

  def get_queryset(self):
    qs = super(CabinetArticlesSubscriptionsView, self).get_queryset()
    #reads = Reader.objects.filter(reader=self.request.user)
    return qs.distinct().filter(
      Q(authors__readees__reader = self.request.user) |
      Q(expert_comments__expert__readees__reader = self.request.user)
    )


class CabinetAuthorsSubscriptionsView(ListView):
  template_name = 'siteuser/cabinet_subscriptions_authors.jhtml'

  def get_context_data(self, **kwargs):
    context = super(CabinetAuthorsSubscriptionsView, self).get_context_data(**kwargs)
    context['part'] = 'subscriptions'
    return context

  def get_queryset(self):
    return User.objects.filter(level__gt = 0, readees__reader = self.request.user)


class CabinetPostArticleView(TemplateView, FormView):
  template_name = 'siteuser/cabinet_post_article.jhtml'

  def get_context_data(self, **kwargs):
    context = super(CabinetPostArticleView, self).get_context_data(**kwargs)
    context['part'] = 'articles'
    return context

  def return_form(self, form):
    return self.render_to_response(self.get_context_data(form=form))

  def post(self, request, *args, **kwargs):
    form = ArticleForm(request.POST)
    if form.is_valid():
      #messages.info(request, u'Запрос на публикацию статьи отправлен редактору')
      instance = form.save(commit=False)
      instance.rubric = Rubric.fetch_rubric('blogi')
      instance.save()
      instance.authors.add(request.user)
      return redirect('siteuser.view.cabinet_articles')
    return self.return_form(form)

  def get(self, request, *args, **kwargs):
    return self.return_form(ArticleForm())
