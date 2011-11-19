from HTMLParser import HTMLParser

from django.core.cache import cache

from pytils.translit import slugify as original_slugify

class MLStripper(HTMLParser):
    def __init__(self):
        self.reset()
        self.fed = []
    def handle_data(self, d):
        self.fed.append(d)
    def get_data(self):
        return ''.join(self.fed)

def strip_tags(html):
    s = MLStripper()
    s.feed(html)
    return s.get_data()

def slugify(text):
    return original_slugify(strip_tags(text))

def cached(func, key, duration=60*60*24):
    res = cache.get(key)
    if res is None:
        res = func()
        cache.set(key, res, duration)
    return res

def cached_method(key, duration=60*60*24):
    def wrapper(func):
        def method(self, *args, **kwargs):
            return cached(lambda: func(self, *args, **kwargs), key.format(**self.__dict__), duration)
        return method
    return wrapper
