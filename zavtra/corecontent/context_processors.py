#-*- coding: utf-8 -*-
from corecontent.models import Rubric, FeaturedItems

from utils import cached

def common_pieces(request):
    rubrics  = cached(lambda: Rubric.objects.exclude(title=u'Новости'), 'rubrics')
    featured = cached(lambda: FeaturedItems.objects.filter(is_active=True), 'featured')
    return {
        'rubrics': rubrics,
        'featured': featured
    }
