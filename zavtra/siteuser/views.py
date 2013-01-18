#-*- coding: utf-8 -*-
from django.views.generic import DetailView, ListView

from zavtra.paginator import QuerySetDiggPaginator as DiggPaginator

from siteuser.models import User

RU_ALPHABET = map(unichr, range(1040,1072))

class UserView(DetailView):
  template_name = 'siteuser/profile.jhtml'

  def get_queryset(self):
    return User.objects.select_related()


class UsersView(ListView):
  template_name = 'siteuser/authors.jhtml'
  paginate_by = 15
  paginator_class = DiggPaginator

  def get_context_data(self, **kwargs):
    context = super(UsersView, self).get_context_data(**kwargs)
    context['alphabet'] = RU_ALPHABET
    context['selected_letter'] = self.letter
    return context

  def get_queryset(self):
    self.letter = self.kwargs.get('letter')
    if self.letter is None:
      self.letter = u"А"
    return User.objects.filter(level__gte = User.USER_LEVELS.trusted).\
           filter(last_name__startswith = self.letter).\
           select_related()
