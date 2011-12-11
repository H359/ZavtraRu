from django.conf.urls.defaults import patterns, url

urlpatterns = patterns('',
    url('^add/$', 'comments.views.add_comment', name='comments.add'),
    url('^delete/$', 'comments.views.delete_comment', name='comments.delete'),
)
