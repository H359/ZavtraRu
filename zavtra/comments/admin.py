from django.contrib import admin


from comments.models import Comment

class CommentAdmin(admin.ModelAdmin):
  list_display = ('article', 'author')
  model = Comment
  raw_id_fields = ('article', 'author')
  autocomplete_lookup_fields = {
    'fk': ['article', 'author']
  }

admin.site.register(Comment, CommentAdmin)
