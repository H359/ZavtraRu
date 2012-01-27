#-*- coding: utf-8 -*-
from datetime import datetime

from django.db import models
from django.template import Template, Context
from django.conf import settings
from django.core.cache import cache

class EmailTemplate(models.Model):
    class Meta:
        verbose_name=u'Шаблон письма'
        verbose_name_plural=u'Шаблоны писем'
    key   = models.CharField(max_length=250, verbose_name=u'Ключ', unique=True, db_index=True)
    title = models.CharField(max_length=250, verbose_name=u'Название')
    subj  = models.CharField(max_length=250, verbose_name=u'Тема письма')
    body  = models.TextField(verbose_name=u'Тело письма')

    def send(self, receivers, sender=None, data={}):
	c = Context(data)
	subject = Template(self.subj).render(c)
	body = Template(self.body).render(c)
	if sender is None:
	    sender = settings.DEFAULT_FROM_EMAIL
	for receiver in receivers:
	    EmailQueue.objects.create(
		from_field=sender, to_field=receiver, subject=subject, body=body
	    )

    def save(self, *args, **kwargs):
	super(EmailTemplate, self).save(*args, **kwargs)
	cache.delete('email-template-%s' % self.key)

    @staticmethod
    def get(key):
	cache_key = 'email-template-%s' % key
	res = cache.get(cache_key)
	if res is None:
	    res = EmailTemplate.objects.get(key=key)
	    cache.set(cache_key, res, 60*60*24)
	return res

class EmailQueue(models.Model):
    class Meta:
	verbose_name=u'Очередь email'
	verbose_name_plural=u'Очередь email'
    from_field  = models.CharField(max_length=250)
    to_field    = models.TextField()
    subject     = models.CharField(max_length=250)
    body        = models.TextField()
    created_at  = models.DateTimeField(auto_now_add=True)
    sent_at     = models.DateTimeField(blank=True, null=True)
    
    def mark_sent(self):
	self.sent_at = datetime.now()
	self.save()