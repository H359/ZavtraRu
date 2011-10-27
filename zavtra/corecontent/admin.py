#-*- coding: utf-8 -*-
from django.contrib import admin
from django import forms

from models import Article, Video, Image, Rubric

class ArticleAdminForm(forms.ModelForm):
    class Meta:
        model = Article
    def __init__(self, *args, **kwargs):
        super(ArticleAdminForm, self).__init__(*args, **kwargs)
        self.fields['content'].widget.attrs['class'] = 'wymeditor'

class VideoAdminForm(forms.ModelForm):
    class Meta:
        model = Video
    content = forms.URLField(verify_exists=True, label=u'Ссылка на страницу с видео на YouTube')

class ImageAdminForm(forms.ModelForm):
    class Meta:
        model = Image
    content = forms.ImageField(label=u'Изображение')

class ArticleAdmin(admin.ModelAdmin):
    class Media:
        js = ('js/wymeditor/wymeditor.fixer.js', 'js/wymeditor/jquery.wymeditor.min.js',)
    form = ArticleAdminForm
    list_display = ('__unicode__', 'published', 'enabled')

class VideoAdmin(admin.ModelAdmin):
    form = VideoAdminForm

class ImageAdmin(admin.ModelAdmin):
    form = ImageAdminForm

class RubricAdmin(admin.ModelAdmin):
    list_display = ('__unicode__', 'on_main', 'on_top')

admin.site.register(Article, ArticleAdmin)
admin.site.register(Video, VideoAdmin)
admin.site.register(Image, ImageAdmin)
admin.site.register(Rubric, RubricAdmin)
