#-*- coding: utf-8 -*-
from datetime import datetime, timedelta
from math import ceil

from pytils.dt import MONTH_NAMES

from django.views.generic import ListView, DetailView
from django.shortcuts import Http404, get_object_or_404
from django.core.urlresolvers import reverse
from django.core.cache import cache
from django.db.models import Min, Max, Count

from diggpaginator import DiggPaginator
#from annoying.decorators import render_to
from utils import MakoViewMixin, render_to
from corecontent.models import Rubric, FeaturedItems, ContentItem, ZhivotovIllustration
from comments.models import Comment

from taggit.models import Tag

class ContentItemView(MakoViewMixin, DetailView):
    template_name       = 'corecontent/view.item.html'
    context_object_name = 'item'
    def get_object(self):
	now = datetime.now()
	qs = ContentItem.batched.batch_select('authors').select_related().filter(enabled=True, pub_date__lte = now)
	return super(ContentItemView, self).get_object(qs)

class RubricView(MakoViewMixin, ListView):
    paginate_by         = 15
    paginator_class     = DiggPaginator
    template_name       = 'corecontent/view.collection.html'
    context_object_name = 'items'
    def get_queryset(self):
	try:
	    self.rubric = Rubric.objects.get(slug=self.kwargs['slug'])
	except Rubric.DoesNotExist:
	    self.rubric = None
	now = datetime.now()
	qs = ContentItem.batched.batch_select('authors').filter(enabled=True, pub_date__lte = now)
	if self.rubric is not None:
	    if self.rubric.children_render > 0:
		subs = self.rubric.get_children(till_leafs=True)
		subs.append(self.rubric)
		qs = qs.filter(rubric__in=subs)
	    else:
		qs = qs.filter(rubric=self.rubric)
        return qs

    def get_context_data(self, **kwargs):
	context = super(RubricView, self).get_context_data(**kwargs)
	context['rubric'] = self.rubric
	if self.rubric is not None:
	    context['title'] = self.rubric
	    context['rss'] = reverse('corecontent.rss.rubric', kwargs={'slug': self.rubric.slug})
	else:
	    context['title'] = u'Блоги'
	return context

class NewsView(MakoViewMixin, DetailView):
    template_name       = 'corecontent/view.item.html'
    context_object_name = 'item'
    def get_object(self):
	now = datetime.now()#.date()
	date = datetime(hour=0, minute=0, second=0, year=int(self.kwargs.get('year')), month=int(self.kwargs.get('month')), day=int(self.kwargs.get('day')))
	#.date()
	if date > now:
	    raise Http404
	qs = ContentItem.batched.batch_select('authors').filter(
	    enabled=True,
	    pub_date__gte=date,
	    slug=self.kwargs.get('slug'),
	    rubric=1,
	)
	return super(NewsView, self).get_object(qs)

class FeaturedView(MakoViewMixin, ListView):
    paginate_by         = 15
    paginator_class     = DiggPaginator
    template_name       = 'corecontent/view.collection.html'
    context_object_name = 'items'
    def get_queryset(self):
        self.featured = get_object_or_404(FeaturedItems, slug=self.kwargs['slug'])
        now = datetime.now()#.date()
        return ContentItem.batched.batch_select('authors').filter(
            enabled=True, pub_date__lte = now, tags__id__in=self.featured.tags.all()
        ).distinct()

    def get_context_data(self, **kwargs):
        context = super(FeaturedView, self).get_context_data(**kwargs)
        context['title'] = self.featured
        context['rss'] = reverse('corecontent.rss.featured', kwargs={'slug': self.featured.slug})
        return context

class TaggedItemsView(MakoViewMixin, ListView):
    paginate_by        = 15
    template_name       = 'corecontent/view.collection.html'
    context_object_name = 'items'
    paginator_class     = DiggPaginator
    def get_queryset(self):
        self.tag = get_object_or_404(Tag, slug=self.kwargs['slug'])
        now = datetime.now()#.date()
        return ContentItem.batched.batch_select('authors').filter(enabled=True, pub_date__lte = now, tags__id=self.tag.id)

    def get_context_data(self, **kwargs):
        context = super(TaggedItemsView, self).get_context_data(**kwargs)
        context['title'] = self.tag
        context['rss'] = reverse('corecontent.rss.tag', kwargs={'slug': self.tag.slug})
        return context

class BlogView(MakoViewMixin, ListView):
    paginate_by         = 15
    template_name       = 'corecontent/view.collection.html'
    paginator_class     = DiggPaginator
    context_object_name = 'items'
    def get_queryset(self):
	now = datetime.now()#.date()
        return ContentItem.batched.batch_select('authors').filter(enabled=True, pub_date__lte = now, rubric=None)
    
    def get_context_data(self, **kwargs):
        context = super(BlogView, self).get_context_data(**kwargs)
        context['title'] = u'Блоги'
        return context

class GalleryView(ListView):
    template_name = 'corecontent/zhivotovillustration.list.html'
    context_object_name = 'items'
    def get_queryset(self):
	return ZhivotovIllustration.objects.order_by('pub_date')
    """
    def get_context_data(self, **kwargs):
	context = super(GalleryView, self).get_context_data(**kwargs)
	dates = ZhivotovIllustration.objects.aggregate(miny=Min('pub_date'), maxy=Max('pub_date'))
	context['dates'] = xrange(dates.get('miny').year, dates.get('maxy').year+1)
	return context
    """

class FeaturedIndexView(MakoViewMixin, ListView):
    #paginate_by         = 15
    template_name       = 'corecontent/featured.index.html'
    #paginator_class     = DiggPaginator
    context_object_name = 'items'
    def get_queryset(self):
	return FeaturedItems.objects.all()

class UnpublishedItemsView(MakoViewMixin, ListView):
    paginate_by         = 15
    template_name       = 'corecontent/view.collection.html'
    paginator_class     = DiggPaginator
    context_object_name = 'items'
    def get_queryset(self):
	now = datetime.now()#.date()
	return ContentItem.batched.batch_select('authors').exclude(rubric=1).filter(enabled=True, pub_date__lte = now, published=False)

    def get_context_data(self, **kwargs):
        context = super(UnpublishedItemsView, self).get_context_data(**kwargs)
        context['rss']  = reverse('corecontent.rss.unpublished')
        context['title'] = u'Красная строка'
        return context

def get_dates(q):
    oneday = timedelta(days=1)
    res = []
    year = []
    month = {}
    nums = 150
    curyear = q[0].year
    curmonth = q[0].month
    local = datetime(year=curyear, month=1, day=1)#.date()
    span = 0
    for date in q:
	#ndate = date - oneday*date.weekday()
	if curmonth != date.month:
	    year.append( (MONTH_NAMES[curmonth-1][1], [(k,month.get(k)) for k in sorted(month.keys())]) )
	    month = {}
	    curmonth = date.month
	if curyear != date.year:
	    res.append( (curyear, year) )
	    year = []
	    curyear = date.year
	    local = datetime(year=curyear, month=1, day=1)#.date()
	ldate = 1 + (date-local).days / 7
	if ldate not in month:
	    month[ldate] = (nums, date)
	    nums += 1
	span = max([span, len(month.keys())])
    year.append( (MONTH_NAMES[curmonth-1][1], [(k,month.get(k)) for k in sorted(month.keys())]) )
    res.append( (curyear, year) )
    return (span, reversed(res))

@render_to('corecontent/archive.html')
def view_archive(request, new=False):
    q = ContentItem.objects.filter(published=True, enabled=True).order_by('pub_date')
    if new:
	astart = datetime(hour=0,minute=0,second=0,year=2011,month=12,day=1)
	q = q.filter(pub_date__gte = astart)
    q = q.values_list('pub_date', flat=True).distinct()
    span, dates = get_dates(q)
    return {'dates': dates, 'span': span, 'new': new}

@render_to('corecontent/issue.html')
def view_issue(request, year, month, day): 
    date = datetime(hour=0,minute=0,second=0,year=int(year),month=int(month),day=int(day))
    oneweek = timedelta(days=7)
    wstart = date
    wend = wstart+oneweek
    qs = ContentItem.batched.batch_select('authors').select_related().filter(pub_date__gte = wstart, pub_date__lt = wend, enabled=True, published=True)
    newsletter = {}
    rubrics = cache.get('rubrics') # context processor loaded them already
    def get_rubric(rubric_id):
	for r in rubrics:
	    if r.id == rubric_id:
		return r
	return None
    for item in list(qs):
	newsletter.setdefault(item.rubric_id, {'rubric': None, 'items': []})
	if newsletter[item.rubric_id]['rubric'] is None:
	    newsletter[item.rubric_id]['rubric'] = get_rubric(item.rubric_id)
	newsletter[item.rubric_id]['items'].append(item)
    newsletter = sorted(newsletter.values(), key=lambda p: p['rubric'].position)
    return {
	'newsletter': newsletter,
	'issue': date
    }

@render_to('corecontent/stats.html')
def stats(request):
    now = datetime.now()
    oneday = timedelta(days=1)
    from django.db import connection
    cursor = connection.cursor()
    cursor.execute("select date_trunc('hour', created_at) as hourly, count(id) from comments_comment where created_at > date '%s' group by hourly order by hourly asc" % (now-oneday).date())
    return {'items': cursor.fetchall()}

view_featured_index = FeaturedIndexView.as_view()
view_item = ContentItemView.as_view()
view_rubric = RubricView.as_view()
view_news = NewsView.as_view()
view_featured = FeaturedView.as_view()
view_blog = BlogView.as_view()
view_items_by_tag = TaggedItemsView.as_view()
view_unpublished = UnpublishedItemsView.as_view()
zhivotov_gallery = GalleryView.as_view()