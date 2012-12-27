from content.models import Rubric, Topic


def common(request):
  return {
    'top_topics': Topic.objects.filter(on_top=True),
    'events_rubrics': Rubric.fetch_rubric('novosti'),
    'blogs_rubrics': Rubric.fetch_rubric('blogi'),
    'wod_rubrics': Rubric.fetch_rubric('wod')
  }