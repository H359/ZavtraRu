from django.contrib import admin

from models import Issue, IssueTypePage, Article, Video, Image, ArticleOnIssueTypePage, Rubric

class ArticleOnIssueTypePageAdmin(admin.TabularInline):
    model = ArticleOnIssueTypePage

class IssueTypePageAdmin(admin.ModelAdmin):
    inlines = [ArticleOnIssueTypePageAdmin]

admin.site.register(Issue)
admin.site.register(IssueTypePage, IssueTypePageAdmin)
admin.site.register(Article)
admin.site.register(Video)
admin.site.register(Image)
admin.site.register(Rubric)
