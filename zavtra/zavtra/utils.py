from django.core.cache import cache


def cached(func, key, duration=60):
  res = cache.get(key)
  if res is None:
    res = func()
    cache.set(key, res, duration)
  return res