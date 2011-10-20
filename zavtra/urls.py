from django.conf.urls.defaults import patterns, include, url
from django.contrib import admin

admin.autodiscover()

urlpatterns = patterns('',
    # Examples:
    # url(r'^$', 'zavtra.views.home', name='home'),
    # url(r'^zavtra/', include('zavtra.foo.urls')),
    url(r'^admin/', include(admin.site.urls)),
)
