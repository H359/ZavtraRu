#-*- coding: utf-8 -*-
from django import forms
from django.utils.safestring import mark_safe

class DualPasswordWidget(forms.Widget):
    pass0_field = '%s_pass0'
    pass1_field = '%s_pass1'

    def __init__(self, *args, **kwargs):
        super(DualPasswordWidget, self).__init__(*args, **kwargs)

    def render(self, name, value, *args, **kwargs):
        out = ( forms.PasswordInput().render(self.pass0_field % name, None),
                forms.PasswordInput().render(self.pass1_field % name, None) )
        return mark_safe('<br />'.join(out))

    def value_from_datadict(self, data, files, name):
        pass0 = data.get(self.pass0_field % name, None)
        pass1 = data.get(self.pass1_field % name, None)
        if pass0 and pass1:
            return (pass0, pass1)
        return None

class DualPasswordField(forms.Field):
    widget = DualPasswordWidget()
    def __init__(self, *args, **kwargs):
        kwargs.setdefault('help_text', u'Введите ваш пароль дважды.')
        super(DualPasswordField, self).__init__(*args, **kwargs)

    def clean(self, value):
        super(DualPasswordField, self).clean(value)
        if value:
            pass0, pass1 = value
            if pass0 == pass1:
                return pass0
        raise forms.ValidationError(u'Пароли не совпадают')