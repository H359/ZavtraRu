from django.conf.urls.defaults import patterns, url
from django.contrib.auth.decorators import user_passes_test

from views import ajaxFieldsResponder

urlpatterns = patterns('',
    url(r'^q/$', user_passes_test(lambda u: u.is_staff)(ajaxFieldsResponder), name='ajaxfields.responder'),
)