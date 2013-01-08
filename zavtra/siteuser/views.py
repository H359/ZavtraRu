from django.views.generic import DetailView

from siteuser.models import User

class UserView(DetailView):
  template_name = 'siteuser/profile.jhtml'

  def get_queryset(self):
    return User.objects.select_related()
