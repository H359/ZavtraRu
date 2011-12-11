#-*- coding: utf-8 -*-
from django.forms import ModelForm, Textarea, HiddenInput, IntegerField

from models import Comment

class CommentForm(ModelForm):
    parent = IntegerField(widget=HiddenInput, required=False)
    class Meta:
        model = Comment
        fields = ('content_type', 'object_id', 'comment')
        widgets = {
            'comment': Textarea(attrs={'cols': 30, 'rows': 10}),
            'content_type': HiddenInput,
            'object_id': HiddenInput,
        }