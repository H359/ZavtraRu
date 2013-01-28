from django.conf.urls import patterns, url


urlpatterns = patterns('',
  url(r'^add/$', 'comments.views.add_comment', name='comments.views.add')
)