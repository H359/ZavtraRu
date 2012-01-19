from django.template import Library

from zavtra.utils import group_list

register = Library()

@register.filter
def mul(val, k):
    return val*float(k)

@register.filter
def imul(val, k):
    return val*k

@register.filter
def idivmod(val, k):
    return divmod(k, val)

@register.filter
def fix_commas(val):
    return unicode(val).replace(',', '.')

@register.filter
def negate(val):
    return -val

@register.filter
def filter_by_field(val, field):
    return filter(lambda w: getattr(w, field), val)

@register.filter
def in_group_of(val, k):
    return group_list(val, k)