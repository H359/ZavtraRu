from django.conf.urls.defaults import patterns, include, url
from django.shortcuts import redirect
from django.contrib import admin
from django.conf import settings
from django.contrib.staticfiles.urls import staticfiles_urlpatterns
from django.views.generic.simple import direct_to_template

from corecontent.sitemap import RubricsSitemap, FeaturedItemsSitemap

from haystack.views import SearchView, search_view_factory
from haystack.forms import HighlightedSearchForm
from haystack.query import SearchQuerySet

cc_sqs = SearchQuerySet().order_by('-pub_date')

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
    url(r'^users/(?P<username>[_\-A-Za-z0-9]+)/$', 'zavtra.views.user'),
    url(r'^admin/', include(admin.site.urls)),
    url(r'^comments/', include('zavtra.comments.urls')),
    url(r'^polls/', include('minipoll.urls')),
    url(r'^taggit_autosuggest/', include('taggit_autosuggest.urls')),

    url(r'^live/$', 'views.live', name='live'),
    url(r'^live/update/$', 'views.live_update', name='live.update'),

    #(r'^search/', include('haystack.urls')),
    url(r'^search/', search_view_factory(view_class=SearchView, searchqueryset=cc_sqs, template='search/search.html', results_per_page=15, form_class=HighlightedSearchForm), name = 'search'),

    url(r'^editorial/', include('editorial.urls')),
    (r'^ajax_filtered_fields/', include('ajax_filtered_fields.urls')),

    url(r'^accounts/', include('siteuser.urls')),
    url(r'^admin/filebrowser/', include('filebrowser.urls')),
    url(r'', include('social_auth.urls')),
)

if settings.DEBUG:
    urlpatterns += staticfiles_urlpatterns()
    urlpatterns += patterns('',
        url(r'^media/(?P<path>.*)$', 'django.views.static.serve', {
            'document_root': settings.MEDIA_ROOT,
        }),
   )
