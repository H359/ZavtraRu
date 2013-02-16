#-*- coding: utf-8 -*-
from django.contrib import admin
from django import forms
from django.forms.widgets import TextInput
from tinymce.widgets import TinyMCE

from content.models import Rubric, Issue, RubricInIssue,\
                           ExpertComment, Topic, Article,\
                           DailyQuote
from content.proxies import News, Wod, Video, Columns


class ExpertCommentAdminInline(admin.StackedInline):
  model = ExpertComment
  max_num = None
  extra = 0


class RubricInIssueAdminInline(admin.StackedInline):
  model = RubricInIssue
  max_num = 9
  extra = 9
  sortable_field_name = 'position'


class ArticleAdminForm(forms.ModelForm):
  class Meta:
    model = Article
  content = forms.CharField(label=u'Текст', widget=TinyMCE(attrs={'cols': 80, 'rows': 30}))


class VideoArticleForm(forms.ModelForm):
  class Meta:
    model = Video
  content = forms.CharField(label=u'Ссылка на YouTube/День-ТВ', widget=TextInput(attrs={'style': 'width: 760px'}))


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

  def queryset(self, request):
    return Article.objects.filter(rubric=Rubric.fetch_rubric('wod'))

  def save_model(self, request, obj, form, change):
    obj.rubric = Rubric.fetch_rubric('wod')
    obj.save()


class NewsAdmin(admin.ModelAdmin):
  exclude = ('authors', 'rubric', 'type', 'selected_at', 'topics')
  list_display = ('title', 'status', 'published_at')
  search_fields = ('title',)
  form = ArticleAdminForm

  def queryset(self, request):
    return News.objects.filter(rubric=Rubric.fetch_rubric('novosti'))

  def save_model(self, request, obj, form, change):
    obj.rubric = Rubric.fetch_rubric('novosti')
    obj.save()


class VideoAdmin(admin.ModelAdmin):
  exclude = ('type', 'selected_at',)
  list_display = ('title', 'status', 'published_at')
  search_fields = ('title',)
  form = VideoArticleForm

  def queryset(self, request):
    return Article.objects.filter(type=Article.TYPES.video)

  def save_model(self, request, obj, form, change):
    obj.type = Article.TYPES.video
    obj.save()


class ColumnsAdmin(admin.ModelAdmin):
  exclude = ('rubric', 'selected_at', 'authors')
  list_display = ('title', 'status', 'published_at')
  search_fields = ('title',)
  raw_id_fields = ('authors', 'topics')
  form = ArticleAdminForm
  autocomplete_lookup_fields = {
    'm2m': ['authors', 'topics']
  }

  def queryset(self, request):
    return Article.columns.all()

  def save_model(self, request, obj, form, change):
    obj.rubric = Rubric.fetch_rubric('columnists')
    obj.save()


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
admin.site.register(Video, VideoAdmin)
admin.site.register(Topic, TopicAdmin)
admin.site.register(Columns, ColumnsAdmin)
admin.site.register(Rubric)
admin.site.register(Issue, IssueAdmin)
admin.site.register(DailyQuote)
