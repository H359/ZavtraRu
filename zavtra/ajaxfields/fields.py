from django import forms
from django.forms.util import ValidationError

from widgets import AjaxFilteringSelect, AjaxFilteringSelectMultiple

class AjaxFieldMixin(object):
    def _prepare_params(self, *args, **kwargs):
	self.guard = kwargs.pop('guard', None)
	objcount = kwargs.pop('count', 20)
	queryset = self.model.objects.all()
	if self.guard:
	    queryset = queryset.filter(**self.guard)
	return (queryset[0:objcount], args, kwargs)

class AjaxForeignKeyField(forms.ModelChoiceField, AjaxFieldMixin):
    def __init__(self, model, fields, *args, **kwargs):
	"""
	    (required) model - related model
	    (required) fields - tuple of fields to match agains
	    (optional) count - initial items count, default=20
	    (optional) guard - queryset filter dict
	"""
	self.model = model
	qs, args, kwargs = self._prepare_params(*args, **kwargs)
	widget = AjaxFilteringSelect
	widget.model = model
	widget.fields = fields
	widget.guard = self.guard
	super(AjaxForeignKeyField, self).__init__(queryset=qs, widget=widget, *args, **kwargs)

class AjaxManyToManyField(forms.ModelMultipleChoiceField, AjaxFieldMixin):
    def __init__(self, model, fields, *args, **kwargs):
	"""
	    (required) model - related model
	    (required) fields - tuple of fields to match agains
	    (optional) count - initial items count, default=20
	    (optional) guard - queryset filter dict
	"""
	self.model = model
	qs, args, kwargs = self._prepare_params(*args, **kwargs)
	widget = AjaxFilteringSelectMultiple
	widget.model = model
	widget.fields = fields
	widget.guard = self.guard
	super(AjaxManyToManyField, self).__init__(queryset=qs, widget=widget, *args, **kwargs)

    def clean(self, value):
	if self.required and not value:
	    return ValidationError('required')
	elif not self.required and not value:
	    return []
	return self.model.objects.filter(pk__in = value)