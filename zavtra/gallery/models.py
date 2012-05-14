from django.db import models
from django.contrib.auth.models import User

from model_utils import TitledSlugEntry, WithDenormalizedStats

class Gallery(TitledSlugEntry, WithDenormalizedStats):
    owner  = models.ForeignKey(User)
    hidden = models.BooleanField()

class Image(TitledSlugEntry, WithDenormalizedStats):
    gallery  = models.ForeignKey(Gallery)
    image    = models.ImageField()
    pub_date = models.DateTimeField()