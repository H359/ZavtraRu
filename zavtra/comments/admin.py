from django.contrib import admin


from comments.models import Comment

class CommentAdmin(admin.ModelAdmin):
  list_display = ('comment', 'article', 'author')
  search_fields = ('article__title', 'author__first_name', 'author__last_name')
  model = Comment
  list_select_related = True
  raw_id_fields = ('article', 'author')
  autocomplete_lookup_fields = {
    'fk': ['article', 'author']
  }

admin.site.register(Comment, CommentAdmin)
