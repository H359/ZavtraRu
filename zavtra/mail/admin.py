from django.contrib import admin
from models import EmailTemplate, EmailQueue

class EmailTemplateAdmin(admin.ModelAdmin):
    list_display = ('title', 'key',)
    def has_delete_permission(self, request, obj=None):
	return False

class EmailQueueAdmin(admin.ModelAdmin):
    list_display = ('to_field', 'subject', 'body', 'sent_at', 'created_at')
    def has_delete_permission(self, request, obj=None):
	return False

    def has_add_permission(self, request, obj=None):
	return False

admin.site.register(EmailTemplate, EmailTemplateAdmin)
admin.site.register(EmailQueue, EmailQueueAdmin)