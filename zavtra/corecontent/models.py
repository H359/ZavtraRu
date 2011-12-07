#-*- coding: utf-8 -*-
import urlparse
import urllib2
import random
from datetime import datetime

from django.utils.encoding import smart_str, force_unicode
from django.core.files import File

from django.db import models
from django.contrib.auth.models import User
from django.contrib.contenttypes.models import ContentType
from django.core.files.base import ContentFile

from batch_select.models import BatchManager
from voting.models import Vote
#from taggit.managers import TaggableManager
from taggit_autosuggest.managers import TaggableManager
from taggit.models import Tag
from autoslug import AutoSlugField
from pytils import dt

from utils import cached, cached_method

from comments.models import Comment

@models.permalink
def tag_url(tag):
    return ('corecontent.view.items_by_tag', (), {'slug': tag.slug})
Tag.get_absolute_url = tag_url

class Rubric(models.Model):
    class Meta:
        verbose_name=u'Рубрика'
        verbose_name_plural=u'Рубрики'
        ordering = ['position', '-id']
    title    = models.CharField(max_length=250, verbose_name=u'Заголовок')
    slug     = AutoSlugField(populate_from=lambda instance: instance.title, unique=True, db_index=False)
    on_main  = models.BooleanField(default=False, verbose_name=u'Выводить на главной')
    on_top   = models.BooleanField(default=False, verbose_name=u'Выводить в верхнем большом меню')
    position = models.PositiveIntegerField(verbose_name=u'Положение', default=lambda: Rubric.objects.count()+1)
    
    def __unicode__(self):
        return self.title

    @cached_method('rubric-{id}-items', duration=60)
    def get_content_items(self):
        return ContentItem.batched.batch_select('authors').filter(enabled=True).filter(rubric=self)[0:3]

    @models.permalink
    def get_absolute_url(self):
        return ('corecontent.view.rubric', (), {'slug': self.slug})

class FeaturedItems(models.Model):
    class Meta:
        verbose_name=u'Горячая тема'
        verbose_name_plural=u'Горячие темы'

    title     = models.CharField(max_length=200, verbose_name=u'Заголовок')
    slug      = AutoSlugField(populate_from=lambda instance: instance.title, unique=True, db_index=False)
    is_active = models.BooleanField(verbose_name=u'Выводить на главной / актуально', default=True)
    tags      = models.ManyToManyField(Tag, verbose_name=u'Теги', blank=True)

    def __unicode__(self):
        return self.title

    @models.permalink
    def get_absolute_url(self):
        return ('corecontent.view.featured', (), {'slug': self.slug})

class ContentItem(models.Model):
    class Meta:
        ordering = ['-pub_date', '-id']
    title        = models.CharField(max_length=250, verbose_name=u'Заголовок')
    slug         = AutoSlugField(populate_from=lambda instance: instance.title, unique=True, db_index=False)
    subtitle     = models.CharField(max_length=250, verbose_name=u'Подзаголовок', blank=True)
    rubric       = models.ForeignKey(Rubric, verbose_name=u'Рубрика', blank=True, null=True)
    description  = models.TextField(verbose_name=u'Анонс', blank=True)
    pub_date     = models.DateField(verbose_name=u'Дата публикации', default=datetime.now)
    authors      = models.ManyToManyField(User, verbose_name=u'Авторы', related_name='contentitems', blank=True)
    published    = models.BooleanField(verbose_name=u'Опубликовано в газете')
    enabled      = models.BooleanField(verbose_name=u'Допущено к публикации на сайте', default=True)
    thumbnail    = models.ImageField(upload_to='content/thumbs', verbose_name=u'Эскиз / маленькое изображение', blank=True)
    kind         = models.CharField(max_length=200, editable=False)
    content      = models.TextField(verbose_name=u'Содержимое', blank=True)
    old_url      = models.URLField(verify_exists=True, null=True, blank=True, verbose_name=u'URL на старом сайте')
    
    _comments_count = models.IntegerField(default=0, editable=False)

    tags    = TaggableManager(blank=True)
    batched = BatchManager()
    objects = models.Manager()

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
    @cached_method('contentitem-{id}-votes')
    def rating(self):
        return Vote.objects.get_score(self)['score']
    
    @rating.setter
    def rating(self, value):
        pass
    
    @property
    def comments_count(self):
        return self._comments_count
    
    def update_comments_count(self):
        self._comments_count = Comment.objects.filter(
                content_type = contentitem_ctype_id,
                object_id=self.id,
                enabled=True
        ).count()
        self.save()
    
    @models.permalink
    def get_absolute_url(self):
        return ('corecontent.view.item', (), {'slug': self.slug})

    def __unicode__(self):
        return u'%s (%s)' % (self.title, dt.ru_strftime(date=self.pub_date)) 
    __unicode__.allow_tags = True

    def save(self, *args, **kwargs):
        if self.id is None:
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
        super(ContentItem, self).save(*args, **kwargs)        

class DailyQuote(models.Model):
    class Meta:
	verbose_name=u'Цитата дня'
	verbose_name_plural=u'Цитаты дня'
    quote  = models.TextField(verbose_name=u'Цитата')
    source = models.ForeignKey(ContentItem, verbose_name=u'Источник цитаты')
    day    = models.DateField(verbose_name=u'День', unique=True)

    def __unicode__(self):
	return u'%s' % (self.quote)

    def get_absolute_url(self):
	return self.source.get_absolute_url()

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
	super(NewsItem, self).save(*args, **kwargs)

class Video(ContentItem):
    class Meta:
        verbose_name=u'Видео'
        verbose_name_plural=u'Видео'
        proxy = True
    media   = 'video'
    objects = video_manager()

    def get_video_id(self):
        return urlparse.parse_qs(self.content).values()[0][0]

    def save(self, *args, **kwargs):
        import gdata.youtube.service
        video_id = self.get_video_id()
        yt_service = gdata.youtube.service.YouTubeService()
        entry = yt_service.GetYouTubeVideoEntry(video_id=video_id)
        name = urlparse.urlparse(entry.media.thumbnail[0].url).path.split('/')[-1]
        content = ContentFile(urllib2.urlopen(entry.media.thumbnail[0].url).read())
        self.thumbnail.save(name, content, save=False)
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

#contentitem_ctype_id = ContentType.objects.get_for_model(ContentItem).id
import signals