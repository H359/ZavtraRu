#-*- coding: utf-8 -*-
from django.contrib import admin

from siteuser.models import User


class UserAdmin(admin.ModelAdmin):
  search_fields = ('last_name',)

admin.site.register(User, UserAdmin)