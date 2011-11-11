"""Admin for minipoll"""
from django.contrib import admin
from django.utils.translation import ugettext as _

from minipoll.models import Poll, Choice, Vote

class ChoiceInline(admin.TabularInline):
    model = Choice

class PollAdmin(admin.ModelAdmin):
    inlines = [ChoiceInline,]
    date_hierarchy = 'creation_date'
    search_fields = ('title', 'description', 'slug')
    list_filter = ('status', 'creator', 'publication_date')
    list_display = ('title', 'status', 'creator', 'votes')
    fieldsets = ((None, {'fields': ('title', 'description',)}),
                 (_('Status'), {'fields': ('slug', 'status', 'publication_date')}),
                 )
    prepopulated_fields = {'slug': ('title',)}
    actions_on_top = False
    actions_on_bottom = True

    def save_model(self, request, poll, form, change):
        poll.creator = request.user
        poll.save()
    
admin.site.register(Poll, PollAdmin)

class VoteAdmin(admin.ModelAdmin):
    date_hierarchy = 'creation_date'
    list_filter = ('poll', 'choice', 'voter', 'creation_date')
    list_display = ('poll', 'choice', 'creation_date', 'ip_address', 'voter')
    actions_on_top = False
    actions_on_bottom = True

admin.site.register(Vote, VoteAdmin)
