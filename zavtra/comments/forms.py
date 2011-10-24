from django.forms import ModelForm, Textarea, HiddenInput

from models import Comment

class CommentForm(ModelForm):
    class Meta:
        model = Comment
        fields = ('parent', 'content_type', 'object_id', 'comment')
        widgets = {
            'comment': Textarea(attrs={'cols': 30, 'rows': 10}),
            'content_type': HiddenInput,
            'object_id': HiddenInput,
            'parent': HiddenInput
        }
