from django.core.management.base import BaseCommand
from content.models import Article
from siteuser.models import User


class Command(BaseCommand):
  def handle(self, *args, **kwargs):
    processed = []
    for x in User.objects.all():
      processed.append(x.id)
      if len(unicode(x)) == 0: continue
      dups = User.objects.exclude(id__in=processed+[x.id]).filter(first_name__iexact = x.first_name, last_name__iexact = x.last_name)
      if dups.count() > 0:
	for dup in dups:
          print u'%s duplicates %s' % (dup, x)
          if raw_input('Merge? [y/n] ') == 'y':
            cnt = 0
            for a in Article.objects.filter(authors__in = [dup]):
              a.authors.remove(dup)
              a.authors.add(x)
              cnt += 1
            print '-- Processed %d articles' % cnt
	    dup.delete()
