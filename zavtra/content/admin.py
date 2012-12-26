from django.contrib import admin
from mptt.admin import MPTTModelAdmin

from content.models import Rubric, Article


class ArticleAdmin(admin.ModelAdmin):
  date_hierarchy = 'published_at'
  list_display = ('title', 'status', 'rubric',)
  search_fields = ('title',)
  list_filter = ('status', 'rubric')
  filter_vertical = ('authors',)


admin.site.register(Article, ArticleAdmin)
admin.site.register(Rubric, MPTTModelAdmin)