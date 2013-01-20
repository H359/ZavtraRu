from django.contrib import admin

from content.models import Rubric, Article, ExpertComment


class ExpertCommentAdminInline(admin.StackedInline):
  model = ExpertComment
  max_num = None
  extra = 0


class ArticleAdmin(admin.ModelAdmin):
  date_hierarchy = 'published_at'
  list_display = ('title', 'status', 'rubric',)
  search_fields = ('title',)
  list_filter = ('status', 'rubric')
  filter_vertical = ('authors', 'topics')
  inlines = [ExpertCommentAdminInline]

  def queryset(self, request):
    return Article.everything.all()

admin.site.register(Article, ArticleAdmin)
admin.site.register(Rubric)