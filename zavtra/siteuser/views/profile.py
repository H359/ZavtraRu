#-*- coding: utf-8 -*-
from django.views.generic import DetailView, ListView
from django.shortcuts import get_object_or_404
from django.core.urlresolvers import reverse
from django.db.models import Q

from base import FilteredArticlesView
from siteuser.models import User
from comments.models import Comment
from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator


class ProfileView(DetailView):
  template_name = 'siteuser/profile_author.jhtml'
  queryset = User.objects.filter(level__gt = 0).prefetch_related('readees','readers').all()

  def get_context_data(self, **kwargs):
    context = super(ProfileView, self).get_context_data(**kwargs)
    context['part'] = 'profile'
    return context


class ProfileArticlesView(FilteredArticlesView):
  template_name = 'siteuser/profile_articles.jhtml'

  def get_categories(self):
    return (
      (u'все', None, reverse('siteuser.view.profile_articles', kwargs={'pk': self.user.pk})),
      (u'статьи', 'text', reverse('siteuser.view.profile_articles_category', kwargs={'pk': self.user.pk, 'category': 'text'})),
      (u'видео', 'video', reverse('siteuser.view.profile_articles_category', kwargs={'pk': self.user.pk, 'category': 'video'})),
      (u'слова дня', 'wod', reverse('siteuser.view.profile_articles_category', kwargs={'pk': self.user.pk, 'category': 'wod'}))
    )

  def get_context_data(self, **kwargs):
    context = super(ProfileArticlesView, self).get_context_data(**kwargs)
    context['object'] = self.user
    return context

  def get_queryset(self):
    qs = super(ProfileArticlesView, self).get_queryset()
    self.user = get_object_or_404(User, pk=self.kwargs['pk'])
    return qs.filter(Q(authors__in = [self.user]) | Q(expert_comments__expert = self.user))


class ProfileCommentsView(ListView):
  template_name = 'siteuser/profile_comments.jhtml'
  paginate_by = 15
  paginator_class = DiggPaginator

  def get_context_data(self, **kwargs):
    context = super(ProfileCommentsView, self).get_context_data(**kwargs)
    context['part'] = 'comments'
    context['object'] = self.user
    return context

  def get_queryset(self):
    self.user = get_object_or_404(User, pk=self.kwargs['pk'])
    return Comment.enabled.filter(author=self.user).order_by('-created_at')
