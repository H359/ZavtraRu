from django.forms import ModelForm

from models import Thread

class ThreadForm(ModelForm):
    class Meta:
	fields = ('title', 'text')
	model = Thread