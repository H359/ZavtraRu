#-*- coding: utf-8 -*-
from datetime import datetime

from django.views.generic import ListView, DetailView, YearArchiveView
from django.shortcuts import Http404

from corecontent.models import Rubric, FeaturedItems, ContentItem, ZhivotovIllustration

from taggit.models import Tag

class ContentItemView(DetailView):
    #queryset            = ContentItem.batched.batch_select('authors').filter(enabled=True)
    template_name       = 'corecontent/view.item.html'
    context_object_name = 'item'
    def get_object(self):
	now = datetime.now().date()
	qs = ContentItem.batched.batch_select('authors').filter(enabled=True, pub_date__lt = now)
	return super(ContentItemView, self).get_object(qs)

class RubricView(ListView):
    paginate_by         = 15
    template_name       = 'corecontent/view.collection.html'
    context_object_name = 'items'
    def get_queryset(self):
        self.rubric = Rubric.objects.get(slug=self.kwargs['slug'])
        now = datetime.now().date()
        return ContentItem.batched.batch_select('authors').filter(enabled=True, rubric=self.rubric, pub_date__lt = now)
    
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
        now = datetime.now().date()
        return ContentItem.batched.batch_select('authors').filter(
            enabled=True, pub_date__lt = now, tags__id__in=self.featured.tags.all()
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
        now = datetime.now().date()
        return ContentItem.batched.batch_select('authors').filter(enabled=True, pub_date__lt = now, tags__id=self.tag.id)

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
	now = datetime.now().date()
        return ContentItem.batched.batch_select('authors').filter(enabled=True, pub_date__lt = now, rubric=None)
    
    def get_context_data(self, **kwargs):
        context = super(BlogView, self).get_context_data(**kwargs)
        context['type']  = u'Рубрика'
        context['title'] = u'Блоги'
        return context

class GalleryView(YearArchiveView):
    date_field = 'pub_date'
    model      = ZhivotovIllustration
    make_object_list = True
    def get_year(self):
	try:
	    year = super(GalleryView, self).get_year()
	except Http404:
	    year = datetime.now().year
	return year

class FeaturedIndexView(ListView):
    paginate_by         = 15
    template_name       = 'corecontent/featured.index.html'
    context_object_name = 'items'
    def get_queryset(self):
	return FeaturedItems.objects.all()

view_featured_index = FeaturedIndexView.as_view()

view_item = ContentItemView.as_view()
view_rubric = RubricView.as_view()
view_featured = FeaturedView.as_view()
view_blog = BlogView.as_view()
view_items_by_tag = TaggedItemsView.as_view()
zhivotov_gallery = GalleryView.as_view()