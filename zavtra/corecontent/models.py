#-*- coding: utf-8 -*-
from django.db import models
from django.contrib.auth.models import User

from autoslug import AutoSlugField

class Issue(models.Model):
	class Meta:
		verbose_name=u'Выпуск'
		verbose_name_plural=u'Выпуски'
	# TODO: absolute_number & relative_number should be populated automatically
	absolute_number = models.PositiveIntegerField(verbose_name=u'Номер выпуска (сквозной)', unique=True)
	relative_number = models.PositiveIntegerField(verbose_name=u'Номер выпуска (в году)')
	date            = models.DateField(verbose_name=u'Дата выхода выпуска в печать', unique=True)

class IssueTypePage(models.Model):
	class Meta:
		verbose_name=u'Полоса'
		verbose_name_plural=u'Полосы'
		unique_together = ('issue', 'position')
	issue    = models.ForeignKey(Issue, verbose_name=u'Выпуск')
	title    = models.CharField(max_length=200, verbose_name=u'Заголовок')
	position = models.PositiveIntegerField(verbose_name=u'Порядковый номер полосы')

class ContentItem(models.Model):
	"""
	class Meta:
		verbose_name=u'Статья'
		verbose_name_plural='Статьи'
	"""
	title        = models.CharField(max_length=250, verbose_name=u'Заголовок')
	slug         = AutoSlugField(populate_from=lambda instance: '-'.join([instance.title, instance.id]))
	subtitle     = models.CharField(max_length=250, verbose_name=u'Подзаголовок', blank=True)
	description  = models.TextField(verbose_name=u'Описание', blank=True)
	date_pub     = models.DateField(verbose_name=u'Дата публикации')
	authors      = models.ManyToManyField(User, verbose_name=u'Авторы')
	enabled      = models.BooleanField(verbose_name=u'Допущено к публикации')

class Article(ContentItem):
	class Meta:
		verbose_name=u'Статья'
		verbose_name_plural='Статьи'
	content = models.TextField(verbose_name=u'Текст статьи')

class Video(ContentItem):
	class Meta:
		verbose_name=u'Видео'
		verbose_name_plural=u'Видео'
	content = models.CharField(max_length=250, verbose_name=u'URL')

class Image(ContentItem):
	class Meta:
		verbose_name=u'Изображение'
		verbose_name_plural=u'Изображения'
	width   = models.IntegerField(editable=False)
	height  = models.IntegerField(editable=False)
	content = models.ImageField(upload_to='content/images', height_field='height', width_field='width')

class ArticleOnIssueTypePage(models.Model):
	class Meta:
		verbose_name=u'Статья в полосе выпуска'
		verbose_name_plural=u'Статьи в полосах выпусков'
	article  = models.ForeignKey(Article, verbose_name=u'Статья')
	page     = models.ForeignKey(IssueTypePage, verbose_name=u'Полоса в выпуске')
	position = models.PositiveIntegerField(verbose_name=u'Порядковый номер статьи в полосе', unique=True)
