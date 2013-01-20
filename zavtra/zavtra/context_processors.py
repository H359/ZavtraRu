from datetime import datetime

from content.models import Rubric, Topic


def common(request):
  return {
    'now': datetime.now(),
    'top_topics': Topic.objects.filter(on_top=True),
    'wod_rubric': Rubric.fetch_rubric('wod'),
  }