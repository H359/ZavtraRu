#-*- coding: utf-8 -*-
import hashlib
import codecs
from django.core.management.base import BaseCommand
from corecontent.models import User

from pytils.translit import slugify

class Command(BaseCommand):
    def handle(self, *args, **kwargs):
	with codecs.open(args[0], mode='r', encoding='utf-8') as ulist:
	    for u in ulist:
		z = u.split(u' ')
		first_name = z[0]
		last_name = ' '.join(z[1:])
		username = hashlib.md5(u.encode('utf-8')).hexdigest()[0:30]
		uz, c = User.objects.get_or_create(
		    first_name = first_name,
		    last_name  = last_name,
		    is_active  = False,
		    is_staff   = True,
		    password   = '!',
		    username   = username
		)