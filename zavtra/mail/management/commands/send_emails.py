#-*- coding: utf-8 -*-
#from django.utils.encoding import force_unicode
#from django.core.exceptions import ObjectDoesNotExist
from django.core.management.base import BaseCommand
from django.conf import settings
from django.core.mail import EmailMultiAlternatives
from django.utils.html import strip_tags
#from django.db import transaction
from mail.models import EmailQueue

class Command(BaseCommand):
    def handle(self, *args, **kwargs):
	# SET SEMAPHORE!!!
	q = EmailQueue.objects.filter(sent_at=None)[0:50]
	for m in q:
	    msg = EmailMultiAlternatives(m.subject, strip_tags(m.body), m.from_field, [m.to_field])
	    msg.attach_alternative(m.body, 'text/html')
	    msg.send()
	    m.mark_sent()