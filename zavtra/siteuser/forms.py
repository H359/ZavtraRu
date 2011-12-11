# -*- coding: utf-8 -*-
from datetime import datetime

from django import forms
from django.db.models import Q
from django.contrib.auth.models import User
from django.forms.extras.widgets import SelectDateWidget
from django.core.validators import RegexValidator, MinLengthValidator

from captcha.fields import ReCaptchaField
from fields import DualPasswordField
from models import SiteProfile

class RegistrationForm(forms.Form):
    username   = forms.CharField(max_length=30, label=u'Имя пользователя', validators=[RegexValidator(r'^[\w.@+-]+$')])
    password   = DualPasswordField(label=u'Пароль')
    email      = forms.EmailField(label=u'Электронная почта')
    gender     = forms.ChoiceField(label=u'Пол', widget=forms.RadioSelect, choices=SiteProfile.GENDER)
    dob        = forms.DateField(label=u'Дата рождения', widget=SelectDateWidget(years=range(1900, datetime.now().year - 15)))
    location   = forms.CharField(label=u'Страна, город', required=False)
    occupation = forms.CharField(label=u'Род занятий', required=False)
    about      = forms.CharField(label=u'О себе', widget=forms.Textarea(attrs={'rows': 10, 'cols': 20}), required=False)
    captcha    = ReCaptchaField(label=u'Проверим вашу человечность', attrs={'theme':'clean'}, help_text=u'Введите слова с картинки')

    def clean(self):
	cleaned_data = self.cleaned_data
	email = cleaned_data.get('email')
	username = cleaned_data.get('username')
	if User.objects.filter(Q(username=username) | Q(email=email)).count() > 0:
	    print 'w00t?'
	    raise forms.ValidationError(u'Такое имя пользователя или электронную почту использовать, к сожалению, нельзя.')
	return cleaned_data

    def clean_password(self):
	data = self.cleaned_data.get('password')
	data = data.strip()
	if len(data) < 5:
	    raise forms.ValidationError(u'Длина пароля должна быть не менее 5 символов')
	return data