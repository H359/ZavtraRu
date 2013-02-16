from datetime import datetime
from django.conf import settings

from content.models import Rubric, Topic


def common(request):
  return {
    'now': datetime.now(),
    'APP_DOMAIN': settings.APP_DOMAIN,
    'top_topics': Topic.objects.filter(on_top=True),
    'wod_rubric': Rubric.fetch_rubric('wod'),
    'gazette_rubrics': Rubric.get_gazette_rubrics()
  }