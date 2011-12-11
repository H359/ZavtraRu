from django.contrib import admin
#from treebeard.admin import TreeAdmin

from models import Comment

class CommentsAdmin(admin.ModelAdmin):
    list_display = ('short_comment', 'author', 'content_object')
    list_select_related = True
    search_fields = ('comment',)

admin.site.register(Comment, CommentsAdmin)