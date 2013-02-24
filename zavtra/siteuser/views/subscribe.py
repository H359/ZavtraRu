from datetime import datetime
from django.views.generic import RedirectView
from django.shortcuts import get_object_or_404

from siteuser.models import User, Reader


class SubscribeUserView(RedirectView):
  def get(self, request, *args, **kwargs):
    self.url = request.GET.get('next', '/')
    if request.user is not None and request.user.is_authenticated():
      readee = get_object_or_404(User, pk=kwargs['readee'])
      rdr = Reader.objects.filter(author=readee, reader=request.user)
      if rdr.count() == 0:
        Reader.objects.create(author=readee, reader=request.user, subscription_start=datetime.now())
    return super(SubscribeUserView, self).get(request, *args, **kwargs)


class UnSubscribeUserView(RedirectView):
  def get(self, request, *args, **kwargs):
    self.url = request.GET.get('next', '/')
    if request.user is not None and request.user.is_authenticated():
      Reader.objects.filter(author_id=kwargs['readee'], reader=request.user).delete()
    return super(UnSubscribeUserView, self).get(request, *args, **kwargs)