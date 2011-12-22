from django.shortcuts import get_object_or_404
from django.views.generic import ListView
from django.views.generic.edit import CreateView
from django.contrib.auth.decorators import user_passes_test
from django.http import HttpResponseRedirect

from forms import ThreadForm
from models import Thread

class EditorialViewIndex(ListView):
    paginate_by = 15
    template_name = 'editorial/view.index.html'
    def get_queryset(self):
	return Thread.get_root_nodes()

class EditorialViewThread(ListView):
    paginate_by = 15
    template_name = 'editorial/view.thread.html'
    def get_queryset(self):
	self.thread = get_object_or_404(Thread, id=self.kwargs['id'])
	qs = self.thread.get_children()
	return qs
    def post(self, request, *args, **kwargs):
	self.thread = get_object_or_404(Thread, id=self.kwargs['id'])
	text = request.POST.get('text')
	if len(text) > 0:
	    self.thread.add_child(text=text, author=request.user)
	return HttpResponseRedirect(self.thread.get_absolute_url())
    def get_context_data(self, **kwargs):
	context = super(EditorialViewThread, self).get_context_data(**kwargs)
	context['root'] = self.thread
	return context

class EditorialViewCreateThread(CreateView):
    form_class = ThreadForm
    template_name = 'editorial/create.thread.html'
    def get_queryset(self):
	return Thread.object.order_by('id').all()
    def form_valid(self, form):
	data = form.cleaned_data
	data['author'] = self.request.user
	self.object = Thread.add_root(**data)
	self.object.save()
	return HttpResponseRedirect(self.object.get_absolute_url())

staff_required = user_passes_test(lambda u: u.is_staff)

index  = staff_required(EditorialViewIndex.as_view())
thread = staff_required(EditorialViewThread.as_view())
create = staff_required(EditorialViewCreateThread.as_view())
