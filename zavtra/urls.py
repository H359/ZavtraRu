from django.conf.urls.defaults import patterns, include, url
from django.contrib import admin
from django.conf import settings
from django.contrib.staticfiles.urls import staticfiles_urlpatterns

from taggit.models import TagBase
from pytils.translit import slugify

def custom_slugify(tag, i):
    slug = slugify(tag.name)
    if i is not None:
        slug += u'_%s' % i
    return slug
setattr(TagBase, 'slugify', custom_slugify)

admin.autodiscover()

urlpatterns = patterns('',
    # Examples:
    url(r'^$', 'zavtra.views.home', name='home'),
    #url(r'^kind/(\w+)/$', 'zavtra.views.home', name='home-by-kind'),
    #url(r'^archive/', 'zavtra.views.archive', name='home-archive'),
    url(r'^logout/', 'zavtra.views.logout', name='logout'),
    url(r'^logged-in/', 'zavtra.views.logged_in'),
    url(r'^content/', include('zavtra.corecontent.urls')),
    url(r'^admin/', include(admin.site.urls)),
    #url(r'^comments/', include('zavtra.comments.urls')),
    url(r'^polls/', include('minipoll.urls')),

    url(r'', include('social_auth.urls')),
)

if settings.DEBUG:
    urlpatterns += staticfiles_urlpatterns()
    urlpatterns += patterns('',
        url(r'^media/(?P<path>.*)$', 'django.views.static.serve', {
            'document_root': settings.MEDIA_ROOT,
        }),
   )

