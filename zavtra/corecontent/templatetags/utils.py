from django.template import Library

register = Library()

@register.filter
def negate(val):
    return -val
