from corecontent.models import Rubric, FeaturedItems

def common_pieces(request):
    rubrics  = Rubric.objects.all()
    featured = FeaturedItems.objects.filter(is_active=True)
    return {
        'rubrics': rubrics,
        'featured': featured
    }
