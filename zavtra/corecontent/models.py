#-*- coding: utf-8 -*-
from datetime import datetime

from django.db import models
from django.contrib.auth.models import User
from django.contrib.contenttypes.models import ContentType

from djangoratings.fields import RatingField
from taggit.managers import TaggableManager
from taggit.models import Tag
from autoslug import AutoSlugField
from pytils import dt

from comments.models import Comment

class Issue(models.Model):
    class Meta:
        verbose_name=u'Выпуск'
        verbose_name_plural=u'Выпуски'
        ordering = ('-date',)
    # TODO: absolute_number & relative_number should be populated automatically
    absolute_number = models.PositiveIntegerField(verbose_name=u'Номер выпуска (сквозной)')
    relative_number = models.PositiveIntegerField(verbose_name=u'Номер выпуска (в году)')
    date            = models.DateField(verbose_name=u'Дата выхода выпуска в печать', unique=True)
    pdf             = models.FileField(upload_to='issues/pdf', verbose_name=u'PDF', editable=False, blank=True)
    pdf_thumbnail   = models.FileField(upload_to='issues/pdf', verbose_name=u'Снимок PDF', editable=not False, blank=True)

    def get_articles(self):
        return ArticleOnIssueTypePage.objects.select_related().filter(
            page__issue=self,
            article__enabled=True
        ).order_by(
            'page__position',
            'position'
        )

    def __unicode__(self):
        return u'№%s (%s) от %s' % (self.relative_number, self.absolute_number, dt.ru_strftime(date=self.date))

class IssueTypePage(models.Model):
    class Meta:
        verbose_name=u'Полоса'
        verbose_name_plural=u'Полосы'
        unique_together = ('issue', 'position')
    issue    = models.ForeignKey(Issue, verbose_name=u'Выпуск')
    title    = models.CharField(max_length=200, verbose_name=u'Заголовок')
    position = models.PositiveIntegerField(verbose_name=u'Порядковый номер полосы')

    def __unicode__(self):
        return u'"%s" в выпуске %s' % (self.title, self.issue)

class Rubric(models.Model):
    class Meta:
        verbose_name=u'Рубрика'
        verbose_name_plural=u'Рубрики'
    title   = models.CharField(max_length=250, verbose_name=u'Заголовок')
    slug    = AutoSlugField(populate_from=lambda instance: instance.title, unique=True)
    on_main = models.BooleanField(default=False, verbose_name=u'Выводить на главной')
    
    def __unicode__(self):
        return self.title

    #TODO: Should be cached
    def get_content_items(self):
        return ContentItem.objects.filter(enabled=True).filter(rubric=self)[0:3]

class ContentItem(models.Model):
    class Meta:
        ordering = ['-date_pub']
    title        = models.CharField(max_length=250, verbose_name=u'Заголовок')
    slug         = AutoSlugField(populate_from=lambda instance: '-'.join([datetime.now().date().isoformat(), instance.title]), unique=True)
    subtitle     = models.CharField(max_length=250, verbose_name=u'Подзаголовок', blank=True)
    description  = models.TextField(verbose_name=u'Описание', blank=True)
    date_pub     = models.DateField(verbose_name=u'Дата публикации')
    authors      = models.ManyToManyField(User, verbose_name=u'Авторы')
    enabled      = models.BooleanField(verbose_name=u'Допущено к публикации')
    #thumbnail    = models.ImageField(upload_to='content/thumbs', verbose_name=u'Эскиз / маленькое изображение', blank=True, null=True)
    thumbnail    = models.CharField(max_length=200, verbose_name=u'Эскиз / маленькое изображение', blank=True, null=True)
    kind         = models.CharField(max_length=200, editable=False)
    rubric       = models.ForeignKey(Rubric, verbose_name=u'Рубрика', blank=True, null=True)
    _comments_count = models.IntegerField(default=0, editable=False)
    _views_count    = models.IntegerField(default=0, editable=False) # fuzzy views-counter

    tags = TaggableManager()

    @models.permalink
    def get_absolute_url(self):
        return ('corecontent.view.item', (), {'slug': self.slug})

    def __unicode__(self):
        return u'%s от %s' % (self.title, dt.ru_strftime(date=self.date_pub)) 

    def get_views_count(self):
        return self._views_count

    def get_comments(self):
        return Comment.objects.select_related().filter(
            object_id=self.id,
            content_type=ContentType.objects.get_for_model(self)
        )

    def get_comments_count(self):
        return self._comments_count

    def recalculate_comments_count(self):
        ContentItem.objects.filter(id=self.id).update(_comments_count = self.get_comments().count())

class Article(ContentItem):
    class Meta:
        verbose_name=u'Статья'
        verbose_name_plural='Статьи'
    media = 'text'
    content         = models.TextField(verbose_name=u'Текст статьи')

    def save(self, *args, **kwargs):
        super(Article, self).save(*args, **kwargs)
        ContentItem.objects.filter(id=self.id).update(kind=self.media)

class Video(ContentItem):
    class Meta:
        verbose_name=u'Видео'
        verbose_name_plural=u'Видео'
    media = 'video'
    content = models.CharField(max_length=250, verbose_name=u'URL')

    def save(self, *args, **kwargs):
        if self.thumbnail is None or len(self.thumbnail) == 0:
            import gdata.youtube.service, urlparse
            video_id = urlparse.parse_qs(self.content).values()[0][0]
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
    media = 'image'
    width   = models.IntegerField(editable=False)
    height  = models.IntegerField(editable=False)
    content = models.ImageField(upload_to='content/images', height_field='height', width_field='width')

    def save(self, *args, **kwargs):
        super(Image, self).save(*args, **kwargs)
        ContentItem.objects.filter(id=self.id).update(kind=self.media)

class ArticleOnIssueTypePage(models.Model):
	class Meta:
		verbose_name=u'Статья в полосе выпуска'
		verbose_name_plural=u'Статьи в полосах выпусков'
	article  = models.ForeignKey(Article, verbose_name=u'Статья')
	page     = models.ForeignKey(IssueTypePage, verbose_name=u'Полоса в выпуске')
	position = models.PositiveIntegerField(verbose_name=u'Порядковый номер статьи в полосе')
