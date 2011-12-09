#-*- coding: utf-8 -*-
from django.views.generic import ListView, DetailView

from corecontent.models import Rubric, FeaturedItems, ContentItem

from taggit.models import Tag

class ContentItemView(DetailView):
    queryset            = ContentItem.batched.batch_select('authors').filter(enabled=True)
    template_name       = 'corecontent/view.item.html'
    context_object_name = 'item'

class RubricView(ListView):
    paginate_by         = 15
    template_name       = 'corecontent/view.collection.html'
    context_object_name = 'items'
    def get_queryset(self):
        self.rubric = Rubric.objects.get(slug=self.kwargs['slug'])
        return ContentItem.batched.batch_select('authors').filter(enabled=True, rubric=self.rubric)
    
    def get_context_data(self, **kwargs):
        context = super(RubricView, self).get_context_data(**kwargs)
        context['type']  = u'Рубрика'
        context['title'] = self.rubric
        return context

class FeaturedView(ListView):
    paginate_by         = 15
    template_name       = 'corecontent/view.collection.html'
    context_object_name = 'items'
    def get_queryset(self):
        self.featured = FeaturedItems.objects.get(slug=self.kwargs['slug'])
        return ContentItem.batched.batch_select('authors').filter(
            enabled=True, tags__id__in=self.featured.tags.all()
        ).distinct()
    
    def get_context_data(self, **kwargs):
        context = super(FeaturedView, self).get_context_data(**kwargs)
        context['type']  = u'Рубрика'
        context['title'] = self.featured
        return context

class TaggedItemsView(ListView):
    paginate_by        = 15
    template_name       = 'corecontent/view.collection.html'
    context_object_name = 'items'
    def get_queryset(self):
        self.tag = Tag.objects.get(slug=self.kwargs['slug'])
        return ContentItem.batched.batch_select('authors').filter(enabled=True, tags__id=self.tag.id)

    def get_context_data(self, **kwargs):
        context = super(TaggedItemsView, self).get_context_data(**kwargs)
        context['type']  = u'Всё по тегу'
        context['title'] = self.tag
        return context

class BlogView(ListView):
    paginate_by         = 15
    template_name       = 'corecontent/view.collection.html'
    context_object_name = 'items'
    def get_queryset(self):
        return ContentItem.batched.batch_select('authors').filter(enabled=True, rubric=None)
    
    def get_context_data(self, **kwargs):
        context = super(RubricView, self).get_context_data(**kwargs)
        context['type']  = u'Рубрика'
        context['title'] = u'Блоги'
        return context


view_item = ContentItemView.as_view()
view_rubric = RubricView.as_view()
view_featured = FeaturedView.as_view()
view_blog = BlogView.as_view()
view_items_by_tag = TaggedItemsView.as_view()
