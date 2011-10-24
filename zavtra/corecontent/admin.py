from django.contrib import admin
from django.contrib.contenttypes import generic

from models import Issue, IssueTypePage, Article, Video, Image, ArticleOnIssueTypePage
from tags.models import TaggedItem

class TagsInline(generic.GenericStackedInline):
    model = TaggedItem

class ArticleOnIssueTypePageAdmin(admin.TabularInline):
    model = ArticleOnIssueTypePage

class IssueTypePageAdmin(admin.ModelAdmin):
    inlines = [ArticleOnIssueTypePageAdmin]

class ArticleAdmin(admin.ModelAdmin):
    inlines = [TagsInline]

class ImageAdmin(admin.ModelAdmin):
    inlines = [TagsInline]

class VideoAdmin(admin.ModelAdmin):
    inlines = [TagsInline]

admin.site.register(Issue)
admin.site.register(IssueTypePage, IssueTypePageAdmin)
admin.site.register(Article, ArticleAdmin)
admin.site.register(Video, VideoAdmin)
admin.site.register(Image, ImageAdmin)
