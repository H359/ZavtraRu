from django.conf.urls import patterns, url
from django.contrib.auth.decorators import login_required, user_passes_test
from views import csv_view

urlpatterns = patterns('',
  url(r'^(?P<year>[0-9]{4})/(?P<month>[0-9]{1,2})/$', login_required(user_passes_test(lambda u: u.is_staff)(csv_view))),
)
