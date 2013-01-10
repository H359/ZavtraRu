from datetime import datetime

from content.models import Rubric, Topic


def common(request):
  return {
    'now': datetime.now(),
    'top_topics': Topic.objects.filter(on_top=True),
    'news_rubrics': Rubric.fetch_rubric('novosti'),
    'events_rubrics': Rubric.fetch_rubric('events'),
    'blogs_rubrics': Rubric.fetch_rubric('blogi'),
    'wod_rubrics': Rubric.fetch_rubric('wod'),
    'gazette_rubrics': Rubric.fetch_rubric('zeitung')
  }