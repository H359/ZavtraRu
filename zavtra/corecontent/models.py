#-*- coding: utf-8 -*-
import urlparse
import urllib2
import random

from datetime import datetime, timedelta

from django.utils.encoding import smart_str, force_unicode
from django.core.files import File

from django.db import models
from django.contrib.auth.models import User
from django.contrib.contenttypes.models import ContentType
from django.core.files.base import ContentFile
from django.core.cache import cache

from imagekit.models import ImageSpec
from imagekit.processors import resize, Adjust
from batch_select.models import BatchManager
from voting.models import Vote
#from taggit.managers import TaggableManager
from taggit_autosuggest.managers import TaggableManager
from taggit.models import Tag
from autoslug import AutoSlugField
from pytils import dt
from djangosphinx.models import SphinxSearch

from typo import typography

from utils import cached, cached_method

from comments.models import Comment

@models.permalink
def tag_url(tag):
    return ('corecontent.view.items_by_tag', (), {'slug': tag.slug})
Tag.get_absolute_url = tag_url

@models.permalink
def get_user_url(user):
    return ('accounts.view.user', (), {'username': user.username})

User.__unicode__ = lambda s: s.get_full_name() if s.first_name or s.last_name else s.username
User.get_absolute_url = lambda s: get_user_url(s)

class Rubric(models.Model):
    class Meta:
        verbose_name=u'Рубрика'
        verbose_name_plural=u'Рубрики'
        ordering = ['position', '-id']
    title    = models.CharField(max_length=250, verbose_name=u'Заголовок')
    slug     = AutoSlugField(populate_from=lambda instance: instance.title, unique=True, db_index=False, max_length=250)
    on_main  = models.BooleanField(default=False, verbose_name=u'Выводить на главной')
    on_top   = models.BooleanField(default=False, verbose_name=u'Выводить в верхнем большом меню')
    position = models.PositiveIntegerField(verbose_name=u'Положение', default=lambda: Rubric.objects.count()+1)

    def __unicode__(self):
        return self.title

    @models.permalink
    def get_absolute_url(self):
        return ('corecontent.view.rubric', (), {'slug': self.slug})

    def save(self, *args, **kwargs):
	super(Rubric, self).save(*args, **kwargs)
	cache.delete('rubrics')

    def delete(self, *args, **kwargs):
	super(Rubric, self).delete(*args, **kwargs)
	cache.delete('rubrics')

class FeaturedItems(models.Model):
    class Meta:
        verbose_name=u'Горячая тема'
        verbose_name_plural=u'Горячие темы'

    title     = models.CharField(max_length=200, verbose_name=u'Заголовок')
    slug      = AutoSlugField(populate_from=lambda instance: instance.title, unique=True, db_index=False)
    is_active = models.BooleanField(verbose_name=u'Выводить на главной / актуально', default=True)
    tags      = models.ManyToManyField(Tag, verbose_name=u'Теги', blank=True)

    batched = BatchManager()
    objects = models.Manager()

    def __unicode__(self):
        return self.title

    def save(self, *args, **kwargs):
	super(FeaturedItems, self).save(*args, **kwargs)
	cache.delete('featured')

    def delete(self, *args, **kwargs):
	super(FeaturedItems, self).delete(*args, **kwargs)
	cache.delete('featured')

    @models.permalink
    def get_absolute_url(self):
        return ('corecontent.view.featured', (), {'slug': self.slug})

class ContentItem(models.Model):
    class Meta:
        ordering = ['-pub_date', '-id']
    title        = models.CharField(max_length=250, verbose_name=u'Заголовок')
    slug         = AutoSlugField(max_length=250, populate_from=lambda instance: instance.title, unique=True, db_index=False)
    subtitle     = models.CharField(max_length=250, verbose_name=u'Подзаголовок', blank=True)
    rubric       = models.ForeignKey(Rubric, verbose_name=u'Рубрика', blank=True, null=True)
    description  = models.TextField(verbose_name=u'Анонс', blank=True)
    pub_date     = models.DateTimeField(verbose_name=u'Дата публикации', default=datetime.now)
    authors      = models.ManyToManyField(User, verbose_name=u'Авторы', related_name='contentitems', blank=True, limit_choices_to={'is_staff': True})
    published    = models.BooleanField(verbose_name=u'Опубликовано в газете')
    enabled      = models.BooleanField(verbose_name=u'Допущено к публикации на сайте', default=True)
    thumbnail    = models.ImageField(upload_to='content/thumbs', verbose_name=u'Эскиз / маленькое изображение', blank=True)
    kind         = models.CharField(max_length=200, editable=False)
    content      = models.TextField(verbose_name=u'Содержимое', blank=True)
    old_url      = models.CharField(null=True, blank=True, verbose_name=u'URL на старом сайте', max_length=250)
    exclusive    = models.BooleanField(verbose_name=u'Экслюзив', default=False)

    _comments_count = models.IntegerField(default=0, editable=False)
    _base_rating = models.IntegerField(default=0, verbose_name=u'Базовый рейтинг')
    _rating = models.IntegerField(default=0, editable=False)

    tags    = TaggableManager(blank=True)
    batched = BatchManager()
    objects = models.Manager()
    search  = SphinxSearch(
	weights={
	    'title': 100,
	    'subtitle': 80,
	    'description': 75,
	    'content': 50
	},
	mode='SPH_MATCH_PHRASE',
	rankmode='SPH_RANK_PROXIMITY_BM25'
    )

    def by_kind(self):
	res = {}
	for k in self._meta.fields:
	    res[k.name] = getattr(self, k.name)
	if self.kind == 'video':
	    return Video(**res)
	elif self.kind == 'image':
	    return Image(**res)
	else:
	    return Article(**res)

    @property
    #@cached_method('contentitem-{id}-votes')
    def rating(self):
	return self._rating
    
    @property
    def comments_count(self):
        return self._comments_count

    def tags_all(self):
	if hasattr(self, '__cached_tags'):
	    return getattr(self, '__cached_tags')
	else:
	    tags = list(self.tags.all())
	    setattr(self, '__cached_tags', tags)
	    return tags

    def update_comments_count(self):
    	comments_count = Comment.objects.filter(
            content_type = contentitem_ctype_id,
            object_id=self.id,
            enabled=True
    	).count()
    	ContentItem.objects.filter(id=self.pk).update(_comments_count=comments_count)
    	newsletter = cache.get('newsletter')
    	if newsletter:
    	    for group in newsletter:
    		if group['rubric'].id == self.rubric_id:
    		    for article in group['items']:
    			if article.id == self.id:
    			    article._comments_count = comments_count
    			    cache.set('newsletter', newsletter, 60*60*4)
    			    break

    def get_content_type_id(self):
	return contentitem_ctype_id 

    @models.permalink
    def get_absolute_url(self):
	if self.rubric_id == 1:
	    return ('corecontent.view.news', (), {'year': self.pub_date.year, 'month': self.pub_date.month, 'day': self.pub_date.day, 'slug': self.slug})
        return ('corecontent.view.item', (), {'slug': self.slug})

    def __unicode__(self):
        return u'%s (%s)' % (self.title, dt.ru_strftime(date=self.pub_date)) 
    __unicode__.allow_tags = True

    # TODO: DIRTY HACK!!1
    def get_video_id(self):
	q = urlparse.urlparse(self.content).query
	return (urlparse.parse_qs(q).get('v')[0]).strip()

    def delete(self, *args, **kwargs):
	if self.rubric_id is not None and self.rubric_id == 1:
	    cache.delete('news')
	else:
	    cache.delete('red_string')
	super(ContentItem, self).delete(*args, **kwargs)

    def save(self, *args, **kwargs):
	notypo = kwargs.get('notypo', False)
	if notypo:
	    del kwargs['notypo']
	"""
        if not notypo and self.id is None:
            from typograph.RemoteTypograf import RemoteTypograf
            rt = RemoteTypograf()
            rt.htmlEntities()
            rt.br(0)
            rt.p(0)
            rt.nobr(3)
            for field in ['description', 'content']:
                field_val = getattr(self, field)
                field_val = field_val.strip()
                if len(field_val) < 32000:
                    setattr(self, field, force_unicode(rt.processText(smart_str(field_val))))
        """
	self.title = typography(self.title)
	self.description = typography(self.description)
	self.subtitle = typography(self.subtitle)
	"""
	if self.kind == 'text':
	    self.content = typography(self.content)
	"""
        super(ContentItem, self).save(*args, **kwargs)
	if self.rubric_id is not None and self.rubric_id == 1:
	    cache.delete('news')
	else:
	    cache.delete('red_string')


class DailyQuote(models.Model):
    class Meta:
	verbose_name=u'Цитата дня'
	verbose_name_plural=u'Цитаты дня'
    quote  = models.TextField(verbose_name=u'Цитата')
    source = models.ForeignKey(ContentItem, verbose_name=u'Источник цитаты')
    day    = models.DateField(verbose_name=u'День', unique=True, default=lambda: datetime.now())

    def __unicode__(self):
	return u'%s' % (self.quote[0:40])

    def get_absolute_url(self):
	return self.source.get_absolute_url()

class ZhivotovIllustration(models.Model):
    class Meta:
	verbose_name=u'Иллюстрация Животова'
	verbose_name_plural=u'Иллюстрации Животова'
	ordering = ('-pub_date', '-id')
    pub_date  = models.DateField(verbose_name=u'Дата публикации', default=datetime.now)
    title     = models.CharField(max_length=250, verbose_name=u'Название')
    thumbnail = models.ImageField(upload_to='zhivot/thumb/', verbose_name=u'Изображение для главной')
    original  = models.ImageField(upload_to='zhivot/big/', verbose_name=u'Оригинальное Изображение')
    micro     = ImageSpec([Adjust(contrast=1.2, sharpness=1.1), resize.Crop(160,80)],
			  image_field='original', format='JPEG', pre_cache=True)
    for_main  = ImageSpec([resize.Fit(600, 262)],
			   image_field='thumbnail', format='JPEG', pre_cache=True)

    def __unicode__(self):
	return u'%s %s' % (self.title, self.pub_date)

    def get_rus_month_with_year(self):
	return '%s %s' % (dt.MONTH_NAMES[self.pub_date.month-1][1].capitalize(), self.pub_date.year)

""" Specific content items """

def content_manager_for(kind):
    class ContentItemManager(models.Manager):
        def get_query_set(self):
            return super(ContentItemManager, self).get_query_set().filter(kind=kind)
    return ContentItemManager


article_manager = content_manager_for('text')
video_manager = content_manager_for('video')
image_manager = content_manager_for('image')

class Article(ContentItem):
    class Meta:
        verbose_name=u'Статья'
        verbose_name_plural='Статьи'
        proxy = True
    media   = 'text'
    objects = article_manager()

    def save(self, *args, **kwargs):
        super(Article, self).save(*args, **kwargs)
        ContentItem.objects.filter(id=self.id).update(kind=self.media)

class NewsManager(models.Manager):
    def get_query_set(self):
	return super(NewsManager, self).get_query_set().filter(rubric__title=u'Новости')

class NewsItem(ContentItem):
    class Meta:
	verbose_name=u'Новость'
	verbose_name_plural=u'Новости'
	proxy=True

    objects = NewsManager()

    def save(self, *args, **kwargs):
	self.rubric = Rubric.objects.get(title=u'Новости')
	self.kind = 'text'
	super(NewsItem, self).save(*args, **kwargs)

class Video(ContentItem):
    class Meta:
        verbose_name=u'Видео'
        verbose_name_plural=u'Видео'
        proxy = True
    media   = 'video'
    objects = video_manager()

    def get_video_id(self):
	q = urlparse.urlparse(self.content).query
	return (urlparse.parse_qs(q).get('v')[0]).strip()

    def save(self, *args, **kwargs):
	super(Video, self).save(*args, **kwargs)
	ContentItem.objects.filter(id=self.id).update(kind=self.media)

class Image(ContentItem):
    class Meta:
        verbose_name=u'Изображение'
        verbose_name_plural=u'Изображения'
        proxy = True
    media = 'image'
    objects = image_manager()

    def save(self, *args, **kwargs):
        super(Image, self).save(*args, **kwargs)
        ContentItem.objects.filter(id=self.id).update(kind=self.media)

contentitem_ctype_id = ContentType.objects.get_for_model(ContentItem).id
import signals