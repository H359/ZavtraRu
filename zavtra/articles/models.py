# -*- coding: utf-8 -*-
import urllib2
import urlparse

from django.db import models
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes import generic

from mptt.models import MPTTModel
from markdown import markdown
from users.models import User

from utils.models import TitledSlugEntry, WithDenormalizedStats

class Rubric(MPTTModel, TitledSlugEntry):
    parent = models.ForeignKey('self', blank=True, null=True, related_name='children')

    def get_absolute_url(self):
	return ''

class Article(TitledSlugEntry, WithDenormalizedStats):
    sub_title = models.CharField(max_length=1024, blank=True)
    authors   = models.ManyToManyField(User)
    pub_date  = models.DateTimeField()
    published = models.BooleanField()
    enabled   = models.BooleanField()
    announce  = models.TextField()
    rubric    = models.ForeignKey(Rubric)
    exclusive = models.BooleanField()

    # compiled from parts
    content   = models.TextField(editable=False)
	
class ArticlePart(models.Model):
    class Meta:
        ordering = ('position',)
    article        = models.ForeignKey(Article, related_name='parts')
    position       = models.PositiveIntegerField()
    content_type   = models.ForeignKey(ContentType, null=True)
    object_id      = models.IntegerField()
    content_object = generic.GenericForeignKey('content_type', 'object_id')
	
    def save(self, *args, **kwargs):
        content = u''
        for part in self.article.parts.all():
            obj = part.content_object
            if hasattr(obj, 'render_html'):
                content += obj.render_html()
        self.article.content = content
        self.article.save()

class MarkdownText(models.Model):
    text = models.TextField()

    def render_html(self):
        return markdown(self.text)

class HTMLText(models.Model):
    text = models.TextFIeld()
    
    def render_html(self):
        return self.text
        
class YoutubeVideo(models.Model):
    url = models.URLField()

    def get_video_id(self):
        q = urlparse.urlparse(self.url)
        return urlparse.parse_qs(q.query).get('v')[0].strip()
	
    def render_html(self):
        return '<div class="embed_video" data-video-id="%s"></div>' % self.get_video_id()
