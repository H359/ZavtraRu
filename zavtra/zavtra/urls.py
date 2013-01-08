from django.conf.urls import patterns, include, url
from django.conf import settings
from django.contrib import admin

admin.autodiscover()

urlpatterns = patterns('',
    url(r'^$', 'zavtra.views.home', name='home'),
    url(r'^content/', include('content.urls')),
    url(r'^accounts/', include('siteuser.urls')),
    url(r'^grappelli/', include('grappelli.urls')),
    url(r'^admin/', include(admin.site.urls))
)

if settings.DEBUG:
  urlpatterns += patterns('',
    url('^media/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.MEDIA_ROOT}),
  )