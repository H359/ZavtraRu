from django.conf.urls import patterns, url
from django.contrib.auth.decorators import login_required, user_passes_test
from analytics.views import HitsView

urlpatterns = patterns('',
  url(r'', login_required(user_passes_test(lambda u: u.is_staff)(HitsView.as_view()))),
)
