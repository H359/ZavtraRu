from django.db import models
from django.contrib.auth.models import User

from utils.models import TitledSlugEntry, WithDenormalizedStats

class Gallery(TitledSlugEntry, WithDenormalizedStats):
    owner  = models.ForeignKey(User)
    hidden = models.BooleanField()

class Image(TitledSlugEntry, WithDenormalizedStats):
    gallery  = models.ForeignKey(Gallery)
    image    = models.ImageField(upload_to='images/')
    pub_date = models.DateTimeField()