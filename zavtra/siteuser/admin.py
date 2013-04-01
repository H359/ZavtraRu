#-*- coding: utf-8 -*-
from django.contrib import admin

from siteuser.models import User


class UserAdmin(admin.ModelAdmin):
  list_display = ('email', 'first_name', 'last_name', 'date_joined', 'level', 'allow_login')
  list_filter = ('level', 'allow_login') 
  search_fields = ('last_name', 'email')
  exclude = ('password',)

  """
  def save_model(self, request, obj, form, change):
    super(UserAdmin, self).save_model(request, obj, form, change)
    obj.set_password(form.cleaned_data['password'])
    obj.save()
  """

admin.site.register(User, UserAdmin)
