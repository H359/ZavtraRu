from datetime import datetime
from django.conf import settings

from zavtra.utils import cached
from content.models import Rubric, Topic


def common(request):
  return {
    'now': datetime.now(),
    'APP_DOMAIN': settings.APP_DOMAIN,
    'top_topics': cached(lambda: Topic.objects.filter(on_top=True), 'topics:on_top', 3600),
    'wod_rubric': Rubric.fetch_rubric('wod'),
    'gazette_rubrics': Rubric.get_gazette_rubrics()
  }