#-*- coding: utf-8 -*-
from django.contrib import admin
from django import forms
from tinymce.widgets import TinyMCE

from content.models import Rubric, Article, Issue,\
                           ExpertComment, Topic, RubricInIssue,\
                           News, Wod, DailyQuote


class ExpertCommentAdminInline(admin.StackedInline):
  model = ExpertComment
  max_num = None
  extra = 0


class RubricInIssueAdminInline(admin.StackedInline):
  model = RubricInIssue
  max_num = 8
  extra = 8
  sortable_field_name = 'position'


class ArticleAdminForm(forms.ModelForm):
  class Meta:
    model = Article
  content = forms.CharField(label=u'Текст', widget=TinyMCE(attrs={'cols': 80, 'rows': 30}))


class ArticleAdmin(admin.ModelAdmin):
  date_hierarchy = 'published_at'
  list_display = ('title', 'status', 'rubric', 'published_at')
  list_select_related = True
  search_fields = ('title',)
  list_filter = ('status', 'rubric')
  #inlines = [ExpertCommentAdminInline]
  form = ArticleAdminForm
  filter_horizontal = ('authors', 'topics')
  #raw_id_fields = ('authors', 'topics')
  #autocomplete_lookup_fields = {
  #  'm2m': ['authors', 'topics']
  #}


class WodAdmin(admin.ModelAdmin):
  exclude = ('rubric', 'type', 'selected_at')
  list_display = ('title', 'status', 'published_at')
  search_fields = ('title',)
  inlines = [ExpertCommentAdminInline]
  form = ArticleAdminForm
  raw_id_fields = ('authors', 'topics')
  autocomplete_lookup_fields = {
    'm2m': ['authors', 'topics']
  }

  def formfield_for_foreignkey(self, db_field, request=None, **kwargs):
    formfield = super(WodAdmin, self).formfield_for_foreignkey(db_field, request, **kwargs)
    if db_field == 'rubric':
      formfield.queryset = formfield.queryset.filter(slug='wod')
    return formfield


class NewsAdmin(admin.ModelAdmin):
  exclude = ('authors', 'rubric', 'type', 'selected_at', 'topics')
  list_display = ('title', 'status', 'published_at')
  search_fields = ('title',)
  form = ArticleAdminForm

  def formfield_for_foreignkey(self, db_field, request=None, **kwargs):
    formfield = super(NewsAdmin, self).formfield_for_foreignkey(db_field, request, **kwargs)
    if db_field == 'rubric':
      formfield.queryset = formfield.queryset.filter(slug='novosti')
    return formfield


class TopicAdmin(admin.ModelAdmin):
  list_display = ('title', 'position')
  list_editable = ('position',)


class IssueAdmin(admin.ModelAdmin):
  date_hierarchy = 'published_at'
  list_display = ('absolute_number', 'relative_number', 'published_at')
  inlines = [RubricInIssueAdminInline]


admin.site.register(Article, ArticleAdmin)
admin.site.register(News, NewsAdmin)
admin.site.register(Wod, WodAdmin)
admin.site.register(Topic, TopicAdmin)
admin.site.register(Rubric)
admin.site.register(Issue, IssueAdmin)
admin.site.register(DailyQuote)