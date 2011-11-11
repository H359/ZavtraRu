from django.db.models.signals import post_save
from django.core.cache import cache
from django.dispatch import receiver

from django.core.cache import cache

from models import ContentItem

@receiver(post_save, sender=ContentItem, dispatch_uid='zavtra.corecontent.signals')
def update_cache(sender, **kwargs):
    if kwargs['instance'].rubric is not None:
        cache.delete('rubric-%d-items' % kwargs['instance'].id)

"""
from voting.models import Vote

from models import ContentItem, Rubric, contentitem_ctype_id

@receiver(post_save, sender=Rubric, dispatch_uid='zavtra.corecontent.signals')
def update_rubrics(sender, **kwargs):
    kwargs['instance'].reset_content_items()

@receiver(post_save, sender=ContentItem, dispatch_uid='zavtra.corecontent.signals')
def update_cache(sender, **kwargs):
    if kwargs['instance'].rubric is not None:
        key = 'rubric-%d-content-items' % kwargs['instance'].rubric_id
        cache.set(key, ContentItem.objects.batch_select('authors').filter(enabled=True).filter(rubric=kwargs['instance'].rubric)[0:3], 60*60*24)

@receiver(post_save, sender=Vote, dispatch_uid='zavtra.corecontent.signals')
def update_rating(sender, **kwargs):
    if kwargs['instance'].content_type_id == contentitem_ctype_id:
        kwargs['instance'].object.rating = Vote.objects.get_score(kwargs['instance'])['score']
"""
