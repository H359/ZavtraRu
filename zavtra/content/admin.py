#-*- coding: utf-8 -*-
from django.contrib import admin
from django import forms
from django.forms.widgets import TextInput
from tinymce.widgets import TinyMCE

from content.models import Rubric, Issue, RubricInIssue,\
                           ExpertComment, Topic, Article,\
                           DailyQuote, WodCite, SpecialProject
from content.proxies import News, Wod, Video, Column, Editorial


mce_attrs = {
	'plugins': 'paste',
	'theme': 'advanced',
	'paste_auto_cleanup_on_paste': True,
	'theme_advanced_buttons3_add': 'pastetext,pasteword,selectall'
}

class RestrictedImageField(forms.ImageField):
  def __init__(self, *args, **kwargs):
    self.max_upload_size = kwargs.pop('max_upload_size')
    kwargs['label'] = u'%s (макс. размер файла: %d)' % (kwargs['label'], self.max_upload_size)
    super(RestrictedImageField, self).__init__(*args, **kwargs)

  def clean(self, *args, **kwargs):
    data = super(RestrictedImageField, self).clean(*args, **kwargs)
    try:
      if data.size > self.max_upload_size:
        raise forms.ValidationError(u'Размер изображения не может быть больше %d' % self.max_upload_size)
    except AttributeError:
      pass
    return data


class ExpertCommentAdminInline(admin.StackedInline):
  model = ExpertComment
  max_num = None
  extra = 0
  raw_id_fields = ('expert',)
  autocomplete_lookup_fields = {
    'fk': ['expert']
  }


class RubricInIssueAdminInline(admin.StackedInline):
  model = RubricInIssue
  #max_num = 9
  extra = 9
  sortable_field_name = 'position'


class ArticleAdminForm(forms.ModelForm):
  class Meta:
    model = Article
  content = forms.CharField(label=u'Текст', widget=TinyMCE(attrs={'cols': 80, 'rows': 30}, mce_attrs=mce_attrs))
  cover_source = RestrictedImageField(required=False, label=u'Обложка', max_upload_size=200000)


class ColumnAdminForm(forms.ModelForm):
  class Meta:
    model = Column
  title = forms.CharField(label=u'Заголовок', max_length=20, widget=TextInput(attrs={'class': 'vTextField'}))
  subtitle = forms.CharField(label=u'Подзаголовок', max_length=50, widget=TextInput(attrs={'class': 'vTextField'}))
  content = forms.CharField(label=u'Текст', widget=TinyMCE(attrs={'cols': 80, 'rows': 30}, mce_attrs=mce_attrs))
  cover_source = RestrictedImageField(required=False, label=u'Обложка', max_upload_size=200000)

class NewsAdminForm(forms.ModelForm):
  class Meta:
    model = News
  content = forms.CharField(label=u'Текст', widget=TinyMCE(attrs={'cols': 80, 'rows': 30}, mce_attrs=mce_attrs))
  selected_at = forms.DateTimeField(label=u'Выделить', required=False, help_text=u'Дата привязки')
  cover_source = RestrictedImageField(required=False, label=u'Обложка', max_upload_size=131072, help_text=u'Если заполнено -- новость считается событием')


class WodAdminForm(forms.ModelForm):
  class Meta:
    model = Wod
  title = forms.CharField(label=u'Слово', widget=TextInput(attrs={'style': 'width: 760px'}))
  subtitle = forms.CharField(label=u'Заголовок', widget=TextInput(attrs={'style': 'width: 760px'}))
  cover_source = forms.ImageField(label=u'Обложка', required=False)
  content = forms.CharField(label=u'Текст', widget=TinyMCE(attrs={'cols': 80, 'rows': 30}, mce_attrs=mce_attrs))

  def clean(self):
    data = self.cleaned_data
    if data['status'] == Article.STATUS.ready and data['cover_source'] is None:
      raise forms.ValidationError(u'Обложка пустая, невозможно сменить статус на "Готово к публикации"')
    return data


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
  raw_id_fields = ('authors', 'topics')
  autocomplete_lookup_fields = {
    'm2m': ['authors', 'topics']
  }


class WodCiteAdmin(admin.StackedInline):
  model = WodCite
  max_num = 1
  extra = 1


class WodAdmin(admin.ModelAdmin):
  exclude = ('rubric', 'type', 'selected_at')
  list_display = ('title', 'status', 'published_at')
  search_fields = ('title',)
  inlines = [WodCiteAdmin, ExpertCommentAdminInline]
  form = WodAdminForm
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
  exclude = ('authors', 'rubric', 'type',)
  list_display = ('title', 'status', 'published_at')
  search_fields = ('title',)
  form = NewsAdminForm
  raw_id_fields = ('topics',)
  autocomplete_lookup_fields = {
    'm2m': ['topics']
  }

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
  raw_id_fields = ('authors', 'topics')
  autocomplete_lookup_fields = {
    'm2m': ['authors', 'topics']
  }

  def queryset(self, request):
    return Article.objects.filter(type=Article.TYPES.video)

  def save_model(self, request, obj, form, change):
    obj.type = Article.TYPES.video
    obj.save()


class ColumnsAdmin(admin.ModelAdmin):
  exclude = ('rubric',)
  list_display = ('title', 'status', 'published_at', 'selected_at')
  list_editable = ('selected_at',)
  search_fields = ('title',)
  raw_id_fields = ('authors', 'topics')
  form = ColumnAdminForm
  autocomplete_lookup_fields = {
    'm2m': ['authors', 'topics']
  }

  def queryset(self, request):
    return Article.columns.all()

  def save_model(self, request, obj, form, change):
    obj.rubric = Rubric.fetch_rubric('columnists')
    obj.save()


class EditorialAdmin(admin.ModelAdmin):
  exclude = ('rubric', 'selected_at',)
  list_display = ('title', 'status', 'published_at')
  search_fields = ('title',)
  raw_id_fields = ('authors', 'topics')
  form = ArticleAdminForm
  autocomplete_lookup_fields = {
    'm2m': ['authors', 'topics']
  }

  def queryset(self, request):
    return Article.editorial.all()

  def save_model(self, request, obj, form, change):
    obj.rubric = Rubric.fetch_rubric('editorial')
    obj.save()


class RubricAdmin(admin.ModelAdmin):
  list_display = ('title', 'in_rubricator')
  list_editable = ('in_rubricator',)


class TopicAdmin(admin.ModelAdmin):
  list_display = ('title', 'position')
  list_editable = ('position',)


class IssueAdmin(admin.ModelAdmin):
  date_hierarchy = 'published_at'
  list_display = ('absolute_number', 'relative_number', 'published_at')
  inlines = [RubricInIssueAdminInline]


class DailyQuoteAdmin(admin.ModelAdmin):
  list_display = ('day', 'quote', 'source',)
  raw_id_fields = ('source',)
  autocomplete_lookup_fields = {
    'fk': ['source']
  }


class SpecialProjectAdmin(admin.ModelAdmin):
  list_display = ('title', 'date')
  raw_id_fields = ('articles',)
  autocomplete_lookup_fields = {
    'm2m': ['articles']
  }


admin.site.register(Article, ArticleAdmin)
admin.site.register(News, NewsAdmin)
admin.site.register(Wod, WodAdmin)
admin.site.register(Video, VideoAdmin)
admin.site.register(Topic, TopicAdmin)
admin.site.register(Column, ColumnsAdmin)
admin.site.register(Editorial, EditorialAdmin)
admin.site.register(Rubric, RubricAdmin)
admin.site.register(Issue, IssueAdmin)
admin.site.register(DailyQuote, DailyQuoteAdmin)
admin.site.register(SpecialProject, SpecialProjectAdmin)
