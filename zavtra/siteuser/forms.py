# -*- coding: utf-8 -*-
from django import forms
from django.forms import widgets

from tinymce.widgets import TinyMCE

from siteuser.models import User
from content.models import Article

mce_buttons = ',separator,'.join([','.join(x) for x in (
    ('preview', 'removeformat',),
    ('cut', 'copy', 'paste', 'undo', 'redo'),
    ('bullist', 'numlist', 'hr', 'bold', 'italic'),
    ('justifyleft', 'justifycenter', 'justifyright', 'blockquote', 'indent', 'outdent', 'sub', 'sup', 'link'),
    ('image', 'youtube',)
)])

mce_attrs = {
    'plugins': "paste,youtube,autolink,advimage2",
    'imagemanager_contextmenu': True,
    'theme_advanced_buttons1': mce_buttons,
    'cleanup_on_startup': True,
    'custom_undo_redo_levels': 20,
    'entity_encoding': 'raw'
}


class ArticleForm(forms.ModelForm):
    class Meta:
        model = Article
        exclude = (
            'rubric', 'status', 'type', 'published_at', 'selected_at',
            'authors', 'topics', 'cover_source', 'announce', 'show_icon'
        )
        widgets = {
            'title': widgets.TextInput(attrs={'id': 'add_header'}),
            'subtitle': widgets.TextInput(attrs={'id': 'add_subheader'}),
            'content': TinyMCE(attrs={'id': 'add_text'}, mce_attrs=mce_attrs)
        }


class UserInfoForm(forms.ModelForm):
    class Meta:
        model = User
        exclude = (
            'password', 'level', 'email',
            'date_joined', 'last_login', 'photo',
            'allow_login'
        )
        widgets = {
            'bio': widgets.Textarea(attrs={'cols': 28, 'rows': 8})
        }


class RegisterUserForm(forms.Form):
    first_name = forms.CharField(
        max_length=64,
        label=u'Имя'
    )
    last_name = forms.CharField(
        max_length=64,
        label=u'Фамилия'
    )
    email = forms.EmailField(
        label=u'Email',
        error_messages={'invalid': u'Неверный email'}
    )
    password1 = forms.CharField(
        label=u'Пароль',
        widget=forms.PasswordInput
    )
    password2 = forms.CharField(
        label=u'Повторите пароль',
        widget=forms.PasswordInput
    )

    def clean(self):
        cleaned_data = super(RegisterUserForm, self).clean()
        pass1 = cleaned_data.get('password1')
        pass2 = cleaned_data.get('password2')
        if pass1 != pass2:
            self._errors['password2'] = self.error_class([u'Пароли не совпадают'])
        if 'password2' in cleaned_data:
            del cleaned_data['password2']
        email = cleaned_data.get('email')
        if User.objects.filter(email=email).count() > 0:
            self._errors['email'] = self.error_class([u'Такой email использовать нельзя.'])
        return cleaned_data
