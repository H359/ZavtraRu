# -*- coding: utf-8 -*-
import re
from datetime import datetime
from django import forms
from django.forms import widgets
from django.utils import six
from django.utils.safestring import mark_safe
from pytils.dt import MONTH_NAMES


RE_DATE = re.compile(r'(\d{1,2})-(\d{1,2})-(\d{4})$')


class SelectDateWidget(widgets.Widget):
    month_field = '%s_month'
    day_field = '%s_day'
    year_field = '%s_year'
    none_value = (0, '-----')

    def __init__(self, attrs=None, required=False):
        self.attrs = attrs or {}
        self.required = required

    def render(self, name, value, attrs=None):
        try:
            year_val, month_val, day_val = value.year, value.month, value.day
        except AttributeError:
            year_val = month_val = day_val = None
        if isinstance(value, six.string_types):
            match = RE_DATE.match(value)
            if match:
                year_val, month_val, day_val = [int(v) for v in match.groups()]
        choices = [(i, i) for i in xrange(1996, datetime.now().year + 1)]
        year_html = self.create_select(name, self.year_field, value, year_val, choices)
        choices = [(x + 1, y) for x, y in enumerate(map(lambda w: w[2], MONTH_NAMES))]
        month_html = self.create_select(name, self.month_field, value, month_val, choices)
        choices = [(i, i) for i in range(1, 32)]
        day_html = self.create_select(name, self.day_field, value, day_val, choices)
        return mark_safe(''.join([day_html, month_html, year_html]))

    def value_from_datadict(self, data, files, name):
        y = data.get(self.year_field % name)
        m = data.get(self.month_field % name)
        d = data.get(self.day_field % name)
        if y == m == d == "0":
            return None
        if y and m and d:
            return datetime(year=int(y), month=int(m), day=int(d))
        return data.get(name, None)

    def create_select(self, name, field, value, val, choices):
        if 'id' in self.attrs:
            id_ = self.attrs['id']
        else:
            id_ = 'id_%s' % name
        choices.insert(0, self.none_value)
        local_attrs = self.build_attrs(id=field % id_)
        s = widgets.Select(choices=choices)
        select_html = s.render(field % name, val, local_attrs)
        return select_html


class SearchForm(forms.Form):
    query = forms.CharField(
        max_length=64,
        label=u'Запрос',
        required=False,
        widget=widgets.TextInput(attrs={'class': 'search-query2'})
    )
    #start = forms.DateField(label=u'с', widget=SelectDateWidget, required=False)
    #end = forms.DateField(label=u'по', widget=SelectDateWidget, required=False)
