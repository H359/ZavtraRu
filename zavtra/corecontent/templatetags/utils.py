from django.template import Library

register = Library()

@register.filter
def negate(val):
    return -val

@register.filter
def filter_by_field(val, field):
    return filter(lambda w: getattr(w, field), val)
