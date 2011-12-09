#-*- coding: utf-8 -*-
import os
from datetime import datetime

from django.contrib import admin
from django import forms
from django.conf import settings

from models import Article, Video, Image, Rubric, FeaturedItems, NewsItem, DailyQuote, ZhivotovIllustration

class ContentItemMediaMixin(object):
    class Media:
	js = ('js/wymeditor/wymeditor.fixer.js', 'js/wymeditor/jquery.wymeditor.min.js',)

"""
class ArticleAdminForm(forms.ModelForm):
    class Meta:
        model = Article
    def __init__(self, *args, **kwargs):
        super(ArticleAdminForm, self).__init__(*args, **kwargs)
        self.fields['content'].widget.attrs['class'] = 'wymeditor'
"""

class VideoAdminForm(forms.ModelForm):
    class Meta:
        model = Video
        exclude = ('published', 'thumbnail', 'old_url')
    content = forms.URLField(verify_exists=True, label=u'Ссылка на страницу с видео на YouTube')

class ImageAdminForm(forms.ModelForm):
    class Meta:
        model = Image
        exclude = ('published', 'old_url', 'content')
        #fields = ('title', 'subtitle', 'rubric', 'description', 'thumbnail', 'pub_date', 'enabled', 'authors', 'tags')

    thumbnail = forms.ImageField(label=u'Изображение')

class ArticleAdmin(ContentItemMediaMixin, admin.ModelAdmin):
    list_select_related = True
    list_display = ('__unicode__', 'rubric', 'published', 'enabled')

class NewsItemAdmin(ContentItemMediaMixin, admin.ModelAdmin):
    exclude = ('thumbnail', 'old_url', 'authors', 'published', 'rubric')
    def formfield_for_foreignkey(self, db_field, request=None, **kwargs):
	formfield = super(NewsItemAdmin, self).formfield_for_foreignkey(db_field, request, **kwargs)
	if db_field.name == 'rubric':
	    formfield.queryset = formfield.queryset.filter(title=u'Новости')
	return formfield

class VideoAdmin(ContentItemMediaMixin, admin.ModelAdmin):
    form = VideoAdminForm
    list_select_related = True
    list_display = ('__unicode__', 'rubric', 'published', 'enabled')

class ImageAdmin(ContentItemMediaMixin, admin.ModelAdmin):
    form = ImageAdminForm
    list_select_related = True
    list_display = ('__unicode__', 'rubric', 'published', 'enabled')

class RubricAdmin(admin.ModelAdmin):
    list_display = ('__unicode__', 'on_main', 'on_top', 'position')
    list_editable = ('on_main', 'on_top', 'position')
    
    def has_delete_permission(self, request, obj=None):
	if obj is not None:
	    return obj.title != u'Новости'
	return True

class DailyQuoteAdmin(admin.ModelAdmin):
    list_display = ('quote', 'day')

admin.site.register(Article, ArticleAdmin)
admin.site.register(Video, VideoAdmin)
admin.site.register(Image, ImageAdmin)
admin.site.register(NewsItem, NewsItemAdmin)
admin.site.register(Rubric, RubricAdmin)
admin.site.register(FeaturedItems)
admin.site.register(DailyQuote, DailyQuoteAdmin)
admin.site.register(ZhivotovIllustration)