#-*- coding: utf-8 -*-
from datetime import datetime

from django.views.generic import ListView, DetailView, YearArchiveView
from django.shortcuts import Http404, get_object_or_404
from django.core.urlresolvers import reverse

from diggpaginator import DiggPaginator
from corecontent.models import Rubric, FeaturedItems, ContentItem, ZhivotovIllustration

from taggit.models import Tag

class ContentItemView(DetailView):
    #queryset            = ContentItem.batched.batch_select('authors').filter(enabled=True)
    template_name       = 'corecontent/view.item.html'
    context_object_name = 'item'
    def get_object(self):
	now = datetime.now().date()
	qs = ContentItem.batched.batch_select('authors').filter(enabled=True, pub_date__lte = now)
	return super(ContentItemView, self).get_object(qs)

class RubricView(ListView):
    paginate_by         = 15
    paginator_class     = DiggPaginator
    template_name       = 'corecontent/view.collection.html'
    context_object_name = 'items'
    def get_queryset(self):
        self.rubric = get_object_or_404(Rubric, slug=self.kwargs['slug'])
        now = datetime.now().date()
        return ContentItem.batched.batch_select('authors').filter(enabled=True, rubric=self.rubric, pub_date__lte = now)
    
    def get_context_data(self, **kwargs):
        context = super(RubricView, self).get_context_data(**kwargs)
        context['type']  = u'Рубрика'
        context['title'] = self.rubric
        context['rss'] = reverse('corecontent.rss.rubric', kwargs={'slug': self.rubric.slug})
        return context

class NewsView(DetailView):
    template_name       = 'corecontent/view.item.html'
    context_object_name = 'item'
    def get_object(self):
	now = datetime.now().date()
	date = datetime(year=int(self.kwargs.get('year')), month=int(self.kwargs.get('month')), day=int(self.kwargs.get('day'))).date()
	if date > now:
	    raise Http404
	qs = ContentItem.batched.batch_select('authors').filter(
	    enabled=True,
	    pub_date=date,
	    rubric=1,
	)
	print qs
	return super(NewsView, self).get_object(qs)

class FeaturedView(ListView):
    paginate_by         = 15
    paginator_class     = DiggPaginator
    template_name       = 'corecontent/view.collection.html'
    context_object_name = 'items'
    def get_queryset(self):
        self.featured = get_object_or_404(FeaturedItems, slug=self.kwargs['slug'])
        now = datetime.now().date()
        return ContentItem.batched.batch_select('authors').filter(
            enabled=True, pub_date__lte = now, tags__id__in=self.featured.tags.all()
        ).distinct()

    def get_context_data(self, **kwargs):
        context = super(FeaturedView, self).get_context_data(**kwargs)
        context['type']  = u'Рубрика'
        context['title'] = self.featured
        context['rss'] = reverse('corecontent.rss.featured', kwargs={'slug': self.featured.slug})
        return context

class TaggedItemsView(ListView):
    paginate_by        = 15
    template_name       = 'corecontent/view.collection.html'
    context_object_name = 'items'
    def get_queryset(self):
        self.tag = get_object_or_404(Tag, slug=self.kwargs['slug'])
        now = datetime.now().date()
        return ContentItem.batched.batch_select('authors').filter(enabled=True, pub_date__lte = now, tags__id=self.tag.id)

    def get_context_data(self, **kwargs):
        context = super(TaggedItemsView, self).get_context_data(**kwargs)
        context['type']  = u'Всё по тегу'
        context['title'] = self.tag
        context['rss'] = reverse('corecontent.rss.tag', kwargs={'slug': self.tag.slug})
        return context

class BlogView(ListView):
    paginate_by         = 15
    template_name       = 'corecontent/view.collection.html'
    paginator_class     = DiggPaginator
    context_object_name = 'items'
    def get_queryset(self):
	now = datetime.now().date()
        return ContentItem.batched.batch_select('authors').filter(enabled=True, pub_date__lte = now, rubric=None)
    
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
    paginator_class     = DiggPaginator
    context_object_name = 'items'
    def get_queryset(self):
	return FeaturedItems.objects.all()

class UnpublishedItemsView(ListView):
    paginate_by         = 15
    template_name       = 'corecontent/view.collection.html'
    paginator_class     = DiggPaginator
    context_object_name = 'items'
    def get_queryset(self):
	now = datetime.now().date()
	return ContentItem.batched.batch_select('authors').exclude(rubric=1).filter(enabled=True, pub_date__lte = now, published=False)

    def get_context_data(self, **kwargs):
        context = super(UnpublishedItemsView, self).get_context_data(**kwargs)
        context['rss']  = reverse('corecontent.rss.unpublished')
        context['title'] = u'Красная строка'
        return context

view_featured_index = FeaturedIndexView.as_view()

view_item = ContentItemView.as_view()
view_rubric = RubricView.as_view()
view_news = NewsView.as_view()
view_featured = FeaturedView.as_view()
view_blog = BlogView.as_view()
view_items_by_tag = TaggedItemsView.as_view()
view_unpublished = UnpublishedItemsView.as_view()
zhivotov_gallery = GalleryView.as_view()