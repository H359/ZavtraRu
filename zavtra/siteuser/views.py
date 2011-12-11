from django.contrib.auth.models import User
from django.shortcuts import redirect

from annoying.decorators import render_to

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