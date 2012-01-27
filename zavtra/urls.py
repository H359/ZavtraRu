from django.conf.urls.defaults import patterns, include, url
from django.shortcuts import redirect, Http404
from django.contrib import admin
from django.conf import settings
from django.contrib.staticfiles.urls import staticfiles_urlpatterns
from django.views.generic.simple import direct_to_template

from diggpaginator import DiggPaginator, Page, InvalidPage
from haystack.views import SearchView, search_view_factory
from haystack.forms import HighlightedSearchForm
from haystack.query import SearchQuerySet

from corecontent.sitemap import RubricsSitemap, FeaturedItemsSitemap

class CCSearchView(SearchView):
    def __init__(self, *args, **kwargs):
	kwargs['searchqueryset'] = SearchQuerySet().order_by('-pub_date','score')
	kwargs['template'] = 'search/search.html'
	kwargs['results_per_page'] = 15
	kwargs['form_class'] = HighlightedSearchForm
	super(CCSearchView, self).__init__(*args, **kwargs)
    def build_page(self):
	try:
	    page_no = int(self.request.GET.get('page', 1))
	except (TypeError, ValueError):
	    raise Http404
	if page_no < 1:
	    raise Http404
	start_offset = (page_no - 1) * self.results_per_page
	self.results[start_offset:start_offset + self.results_per_page]
	paginator = DiggPaginator(self.results, self.results_per_page)
	try:
	    page = paginator.page(page_no)
	except InvalidPage:
	    raise Http404
	return (paginator, page)

from taggit.models import TagBase
from pytils.translit import slugify

def custom_slugify(self, tag, i=None):
    slug = slugify(tag)
    if i is not None:
        slug += u'_%s' % i
    return slug
TagBase.slugify = custom_slugify

admin.autodiscover()

urlpatterns = patterns('',
    url(r'^$', 'zavtra.views.home', name='home'),
    (r'^index.html$', lambda r: redirect('home', permanent=True)),

    (r'^sitemap\.xml$', 'django.contrib.sitemaps.views.sitemap', {'sitemaps': {
	'rubrics': RubricsSitemap,
	'featured': FeaturedItemsSitemap
    }}),

    url(r'^login/$', 'zavtra.views.login', name='login'),
    url(r'^logout/$', 'zavtra.views.logout', name='logout'),
    url(r'^logged-in/$', 'zavtra.views.logged_in', name='complete'),
    url(r'^content/', include('zavtra.corecontent.urls')),
    #url(r'^users/(?P<username>[_\-A-Za-z0-9]+)/$', 'zavtra.views.user'),
    #url(r'^users/$', include('zavtra.siteuser.urls')),
    url(r'^comments/', include('zavtra.comments.urls')),
    url(r'^polls/', include('minipoll.urls')),
    url(r'^taggit_autosuggest/', include('taggit_autosuggest.urls')),
    url(r'^vote/$', 'zavtra.views.vote', name='vote'),

    url(r'^live/$', 'views.live', name='live'),
    url(r'^live/update/$', 'views.live_update', name='live.update'),

    #(r'^search/', include('haystack.urls')),
    url(r'^search/', search_view_factory(view_class=CCSearchView), name = 'search'),

    url(r'^editorial/', include('editorial.urls')),
    url(r'^resolver/(?P<content_type_id>[0-9]+)/(?P<id>[0-9]+)/$', 'views.resolver', name='resolve_content_object'),

    url(r'^accounts/', include('siteuser.urls')),
    url(r'^admin/ajax_fields/', include('ajaxfields.urls')),
    url(r'^admin/filebrowser/', include('filebrowser.urls')),
    url(r'^admin/', include(admin.site.urls)),
    url(r'', include('social_auth.urls')),
)

if settings.DEBUG:
    urlpatterns += staticfiles_urlpatterns()
    urlpatterns += patterns('',
        url(r'^media/(?P<path>.*)$', 'django.views.static.serve', {
            'document_root': settings.MEDIA_ROOT,
        }),
   )
