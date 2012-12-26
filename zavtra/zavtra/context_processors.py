from content.models import Rubric


def common(request):
  return {
    'top_topics': None,
    'events_rubrics': Rubric.fetch_rubric('novosti'),
    'blogs_rubrics': Rubric.fetch_rubric('blogi')
  }