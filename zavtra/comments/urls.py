from django.conf.urls.defaults import patterns, url


urlpatterns = patterns('',
  url(r'^add/$', 'comments.views.add_comment', name='comments.views.add')
)