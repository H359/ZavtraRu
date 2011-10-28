from django.core.cache import cache

from corecontent.models import Rubric

def common_pieces(request):
    key = 'rubrics-index'
    rubrics = cache.get(key)
    if rubrics is None:
        rubrics = Rubric.objects.all()
        cache.set(key, rubrics)
    return {
        'rubrics': rubrics
    }
