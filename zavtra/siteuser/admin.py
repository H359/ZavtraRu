from django.contrib import admin

from models import SiteProfile

class ProfileAdmin(admin.ModelAdmin):
    list_display = ('user',)

admin.site.register(SiteProfile, ProfileAdmin)