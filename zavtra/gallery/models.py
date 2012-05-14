from django.db import models
from django.contrib.auth.models import User

from model_utils import TitledSlugModel, WithDenormalizedStats

class Gallery(TitledSlugModel, WithDenormalizedStats):
    owner  = models.ForeignKey(User)
    hidden = models.BooleanField()

class Image(TitledSlugModel, WithDenormalizedStats):
    gallery  = models.ForeignKey(Gallery)
    image    = models.ImageField()
    pub_date = models.DateTimeField()