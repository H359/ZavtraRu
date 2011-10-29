#-*- coding: utf-8 -*-
import urlparse
import random
from datetime import datetime

from django.utils.encoding import smart_str, force_unicode
from django.core.cache import cache

from django.db import models
from django.contrib.auth.models import User
from django.contrib.contenttypes.models import ContentType

from batch_select.models import BatchManager
from voting.models import Vote
from taggit.managers import TaggableManager
from taggit.models import Tag
from autoslug import AutoSlugField
from pytils import dt

from comments.models import Comment

class Rubric(models.Model):
    class Meta:
        verbose_name=u'Рубрика'
        verbose_name_plural=u'Рубрики'
        ordering = ['position', '-id']
    title    = models.CharField(max_length=250, verbose_name=u'Заголовок')
    slug     = AutoSlugField(populate_from=lambda instance: instance.title, unique=True)
    on_main  = models.BooleanField(default=False, verbose_name=u'Выводить на главной')
    on_top   = models.BooleanField(default=False, verbose_name=u'Выводить в верхнем большом меню')
    position = models.PositiveIntegerField(verbose_name=u'Положение', default=1)
    
    content_items_cache_key = lambda w: 'rubric-%d-content-items' % w.id
    
    def __unicode__(self):
        return self.title

    # TODO: refactor this!!1
    def reset_content_items(self):
        key = self.content_items_cache_key()
        res = list( ContentItem.objects.batch_select('authors').filter(enabled=True).filter(rubric=self)[0:3] )
        cache.set(key, res, 60*60*24)

    def get_content_items(self):
        key = self.content_items_cache_key()
        res = cache.get(key)
        if res is None:
            res = list( ContentItem.objects.batch_select('authors').filter(enabled=True).filter(rubric=self)[0:3] )
            cache.set(key, res, 60*60*24)
        return res

    @models.permalink
    def get_absolute_url(self):
        return ('corecontent.view.rubric', (), {'slug': self.slug})

class ContentItem(models.Model):
    class Meta:
        ordering = ['-pub_date', '-id']
    title        = models.CharField(max_length=250, verbose_name=u'Заголовок')
    slug         = AutoSlugField(populate_from=lambda instance: instance.title, unique=True)
    subtitle     = models.CharField(max_length=250, verbose_name=u'Подзаголовок', blank=True)
    rubric       = models.ForeignKey(Rubric, verbose_name=u'Рубрика', blank=True, null=True)
    description  = models.TextField(verbose_name=u'Анонс', blank=True)
    pub_date     = models.DateField(verbose_name=u'Дата публикации')
    authors      = models.ManyToManyField(User, verbose_name=u'Авторы', related_name='contentitems')
    published    = models.BooleanField(verbose_name=u'Опубликовано')
    enabled      = models.BooleanField(verbose_name=u'Допущено к публикации на сайте')
    thumbnail    = models.ImageField(upload_to='content/thumbs', verbose_name=u'Эскиз / маленькое изображение', blank=True, null=True)
    kind         = models.CharField(max_length=200, editable=False)
    content      = models.TextField(verbose_name=u'Текст статьи')
    old_url      = models.URLField(verify_exists=True, null=True, blank=True, verbose_name=u'URL на старом сайте')
    
    _comments_count = models.IntegerField(default=0, editable=False)

    tags    = TaggableManager(blank=True)
    objects = BatchManager()

    rating_cache_key = lambda w: 'contentitem-rating-%d' % w.id
   
    @property
    def rating(self):
        key = self.rating_cache_key()
        rating = cache.get(key)
        if rating is None:
            rating = Vote.objects.get_score(self)['score']
            cache.set(key, rating)
        return rating
    
    @rating.setter
    def rating(self, value):
        key = self.rating_cache_key()
        rating = value
        cache.set(key, rating)
    
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
            for field in ['title', 'subtitle', 'description', 'content']:
                field_val = getattr(self, field)
                field_val = field_val.strip()
                if len(field_val) < 32000:
                    setattr(self, field, force_unicode(rt.processText(smart_str(field_val))))
        super(ContentItem, self).save(*args, **kwargs)        

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
        if self.thumbnail is None or len(self.thumbnail) == 0:
            import gdata.youtube.service
            #video_id = urlparse.parse_qs(self.content).values()[0][0]
            video_id = self.get_video_id()
            yt_service = gdata.youtube.service.YouTubeService()
            entry = yt_service.GetYouTubeVideoEntry(video_id=video_id)
            thumbnail = entry.media.thumbnail[0].url
        else:
            thumbnail = self.thumbnail
        super(Video, self).save(*args, **kwargs)
        ContentItem.objects.filter(id=self.id).update(
            kind=self.media,
            thumbnail=thumbnail
        )

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
