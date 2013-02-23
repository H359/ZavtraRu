#-*- coding: utf-8 -*-
from active_filters import Filter, QSIntegerField, QSChoiceField


class ArticlesFilter(Filter):
  year = QSIntegerField(title=u'Год', blank=True, blank_string=u'все', field='published_at')
  month = QSIntegerField(title=u'Месяц', blank=True, blank_string=u'все', field='published_at')
  category = QSChoiceField(title=u'Категория', blank=True, blank_string=u'все',
    choices=(
      (u'статьи', lambda qs: qs.exclude(rubric__slug = 'wod', type=Article.TYPES.video)),
      (u'видео', lambda qs: qs.filter(type=Article.TYPES.video)),
      (u'слово дня', lambda qs: qs.filter(rubric__slug = 'wod'))
    )
  )
