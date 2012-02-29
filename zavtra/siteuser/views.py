from datetime import datetime

from django.contrib.auth.models import User
from django.shortcuts import redirect, get_object_or_404
from django.views.generic import ListView

from utils import MakoViewMixin, render_to
from diggpaginator import DiggPaginator

from models import SiteProfile
from forms import RegistrationForm

@render_to('registration/registration_form.html')
def register(request):
    if request.method == 'POST':
	form = RegistrationForm(request.POST)
	if form.is_valid():
	    data = form.cleaned_data
	    user = User.objects.create_user(username=data.get('username'), email=data.get('email'), password=data.get('password'))
	    profile = SiteProfile.objects.create(
		user=user, dob=data.get('dob'), gender=data.get('gender'), 
		location=data.get('location'), occupation=data.get('occupation'), activation=''
	    )
	    # send email?
	    return redirect('accounts.complete')
    else:
	form = RegistrationForm()
    return {'form': form}

class UserView(MakoViewMixin, ListView):
    paginate_by = 15
    paginator_class = DiggPaginator
    template_name = 'siteuser/user.html'
    def get_queryset(self):
	self.user = get_object_or_404(User, username=self.kwargs.get('username'))
	part = self.kwargs.get('part')
	if part == 'comments':
	    self.template_name = 'siteuser/comments.html'
	    if self.request.GET.get('page') == '0':
		self.kwargs['page'] = 1
	    return self.user.comments.filter(enabled=True).order_by('-created_at')
	elif part == 'articles':
	    self.template_name = 'siteuser/articles.html'
	    now = datetime.now()
	    if self.request.GET.get('page') == '0':
		self.kwargs['page'] = 1
	    return self.user.contentitems.filter(enabled=True, pub_date__lte = now)
	else:
	    return User.objects.none()

    def get_context_data(self, **kwargs):
	context = super(UserView, self).get_context_data(**kwargs)
	context['ruser'] = self.user
	return context

user = UserView.as_view()