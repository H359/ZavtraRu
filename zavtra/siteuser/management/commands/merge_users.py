from django.core.management.base import BaseCommand
from content.models import Article
from siteuser.models import User


class Command(BaseCommand):
  def handle(self, *args, **kwargs):
    user_from_id, user_to_id = args
    user_from = User.objects.get(id=user_from_id)
    user_to = User.objects.get(id=user_to_id) 
    for a in Article.objects.filter(authors__in = [user_from]):
      a.authors.remove(user_from)
      a.authors.add(user_to)
    user_from.delete()

