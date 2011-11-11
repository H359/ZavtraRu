"""Managers for minipoll"""
from datetime import datetime
from django.db import models

DRAFT = 0
PUBLISHED = 1
ARCHIVED = 2

class PollPublishedManager(models.Manager):
    """Manager to retrieve published polls"""

    def get_query_set(self):
	return super(PollPublishedManager, self).get_query_set().filter(status=PUBLISHED, publication_date__lte=datetime.now())
