# Create your views here.
from django.views.generic import ListView, DetailView

from corecontent.models import Rubric, ContentItem

class ContentItemView(DetailView):
    queryset            = ContentItem.objects.batch_select('authors').filter(enabled=True)
    template_name       = 'corecontent/view.item.html'
    context_object_name = 'item'

class RubricView(ListView):
    paginate_by         = 2
    template_name       = 'corecontent/view.rubric.html'
    context_object_name = 'items'
    def get_queryset(self):
        self.rubric = Rubric.objects.get(slug=self.kwargs['slug'])
        return ContentItem.objects.batch_select('authors').filter(enabled=True, rubric=self.rubric)
    
    def get_context_data(self, **kwargs):
        context = super(RubricView, self).get_context_data(**kwargs)
        context['rubric'] = self.rubric
        return context

view_item = ContentItemView.as_view()
view_rubric = RubricView.as_view()
