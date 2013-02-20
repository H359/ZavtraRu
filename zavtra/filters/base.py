from django.utils import six


class FilterBase(type):
  def __new__(cls, name, bases, attrs):
    super_new = super(FilterBase, cls).__new__
    parents = [b for b in bases if isinstance(b, FilterBase) and
               not (b.__name__ == 'NewBase' and b.__mro__ == (b, object))]
    if not parents:
      return super_new(cls, name, bases, attrs)
    
    module = attrs.pop('__module__')
    new_class = super_new(cls, name, bases, {'__module__': module})
    new_class._filters = []
    for obj_name, obj in attrs.items():
      new_class._filters.append((obj_name, obj))
      setattr(new_class, obj_name, None)
    return new_class


class Filter(six.with_metaclass(FilterBase)):
  def __init__(self, request, queryset):
    self._request = request
    self._queryset = queryset

  def __iter__(self):
    for f in self._filters:
      yield 

  def as_queryset(self):
    return self._queryset

  def next(self):
    raise StopIteration


class FilterItem(object):
  selected_item = None
  title = u'Unnamed field'

  def __init__(self, *args, **kwargs):
    for k, v in kwargs.items():
      setattr(self, k, v)

  def __unicode__(self):
    return self.title