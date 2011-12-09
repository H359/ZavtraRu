from django.conf.urls.defaults import patterns, include, url
from django.contrib import admin
from django.conf import settings
from django.contrib.staticfiles.urls import staticfiles_urlpatterns
from django.views.generic.simple import direct_to_template

from taggit.models import TagBase
from pytils.translit import slugify
from forms import ReCaptchaForm

def custom_slugify(self, tag, i=None):
    slug = slugify(tag)
    if i is not None:
        slug += u'_%s' % i
    return slug
TagBase.slugify = custom_slugify

admin.autodiscover()

urlpatterns = patterns('',
    url(r'^$', 'zavtra.views.home', name='home'),
    url(r'^login/$', 'zavtra.views.login', name='login'),
    url(r'^logout/$', 'zavtra.views.logout', name='logout'),
    url(r'^logged-in/$', 'zavtra.views.logged_in', name='complete'),
    url(r'^content/', include('zavtra.corecontent.urls')),
    url(r'^users/(?P<username>[_\-A-Za-z0-9]+)/$', 'zavtra.views.user'),
    url(r'^admin/', include(admin.site.urls)),
    url(r'^comments/', include('zavtra.comments.urls')),
    url(r'^polls/', include('minipoll.urls')),
    url(r'^taggit_autosuggest/', include('taggit_autosuggest.urls')),

    #url(r'^accounts/', include('registration.backends.default.urls')),
    url(r'^accounts/activate/complete/$', direct_to_template, {'template': 'registration/activation_complete.html'}, name='registration_activation_complete'),
    url(r'^accounts/activate/(?P<activation_key>\w+)/$', 'registration.views.activate', {'backend': 'registration.backends.default.DefaultBackend'}, name='registration_activate'),
    url(r'^accounts/register/$', 'registration.views.register', {'form_class': ReCaptchaForm, 'backend': 'registration.backends.default.DefaultBackend'}, name='registration_register'),
    url(r'^accounts/register/complete/$', direct_to_template, {'template': 'registration/registration_complete.html'}, name='registration_complete'),
    url(r'^accounts/register/closed/$', direct_to_template, {'template': 'registration/registration_closed.html'}, name='registration_disallowed'),
    (r'^accounts/', include('registration.auth_urls')),
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

