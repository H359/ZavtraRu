#-*- coding: utf-8 -*-
from django.contrib import admin

from siteuser.models import User, GoldenAuthor


class UserAdmin(admin.ModelAdmin):
  list_display = ('email', 'first_name', 'last_name', 'date_joined', 'level', 'allow_login')
  list_filter = ('level', 'allow_login')
  search_fields = ('last_name', 'email')
  exclude = ('password', 'resume')

  """
  def save_model(self, request, obj, form, change):
    super(UserAdmin, self).save_model(request, obj, form, change)
    obj.set_password(form.cleaned_data['password'])
    obj.save()
  """


class GoldenAuthorAdmin(admin.ModelAdmin):
    list_display          = ('user', 'position')
    list_editable         = ('position',)
    raw_id_fields         = ('user',)
    autocomplete_lookup_fields = {
        'fk': ['user']
    }

admin.site.register(User, UserAdmin)
admin.site.register(GoldenAuthor, GoldenAuthorAdmin)
