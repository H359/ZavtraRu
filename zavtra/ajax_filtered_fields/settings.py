from django.conf import settings
from django.contrib.admin.views.decorators import staff_member_required

# Set AJAX_FILTERED_FIELDS_AUTH_DECORATOR = None
# in your project settings if you want to give public
# access to the views.json_index view.
# Otherwise set it as a auth decorator callable
# (eg: django.contrib.auth.decorators.login_required).
# Default is django.contrib.admin.views.decorators.staff_member_required.

AUTH_DECORATOR = getattr(settings, 
    "AJAX_FILTERED_FIELDS_AUTH_DECORATOR", 
    staff_member_required)
