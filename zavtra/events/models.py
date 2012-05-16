from django.db import models

from markitup.fields import MarkupField

from utils.models import TitledSlugEntry, WithDenormalizedStats

class Event(TitledSlugEntry, WithDenormalizedStats):
    event_start = models.DateTimeField()
    event_end   = models.DateTimeField(null=True, blank=True)
    location    = models.CharField(blank=True, max_length=1024)
    lat         = models.DecimalField(max_digits=13, decimal_places=10)
    lon         = models.DecimalField(max_digits=13, decimal_places=10)
    description = MarkupField()