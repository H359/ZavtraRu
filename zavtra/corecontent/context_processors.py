from corecontent.models import Rubric

def common_pieces(request):
    return {
        'rubrics': Rubric.objects.all()
    }
