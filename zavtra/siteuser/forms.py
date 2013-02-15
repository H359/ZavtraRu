#-*- coding: utf-8 -*-
from django import forms

from siteuser.models import User


class RegisterUserForm(forms.Form):
  first_name = forms.CharField(max_length=64, label=u'Имя')
  last_name = forms.CharField(max_length=64, label=u'Фамилия')
  email = forms.EmailField(label=u'Email', error_messages={'invalid': u'Неверный email'})
  password1 = forms.CharField(label=u'Пароль', widget=forms.PasswordInput)
  password2 = forms.CharField(label=u'Повторите пароль', widget=forms.PasswordInput)

  def clean(self):
    cleaned_data = super(RegisterUserForm, self).clean()
    pass1 = cleaned_data.get('password1')
    pass2 = cleaned_data.get('password2')
    if pass1 != pass2:
      self._errors['password2'] = self.error_class([u'Пароли не совпадают'])
      if 'password2' in cleaned_data:
        del cleaned_data['password2']
    return cleaned_data