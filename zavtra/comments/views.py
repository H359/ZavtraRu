from django.shortcuts import redirect
from django.views.generic import RedirectView

from comments.forms import CommentForm
from comments.models import Comment, CommentVote


def add_comment(request):
  if request.method == 'POST' and \
    request.user and request.user.is_authenticated() and request.user.level > 0:
    form = CommentForm(request.POST)
    if form.is_valid():
      comment = form.save(commit=False)
      comment.author = request.user
      comment.save()
    return redirect(request.POST.get('next'))


class CommentVoteView(RedirectView):
  def get(self, request, *args, **kwargs):
    self.url = '/'
    if request.user is not None and request.user.is_authenticated():
      obj = Comment.objects.select_related().get(id=self.kwargs['id'])
      self.url = obj.article.get_absolute_url()
      vote, _ = CommentVote.objects.get_or_create(user=request.user, comment=obj)
      vote.vote = 1 if self.kwargs['vote'] == 'up' else -1
      vote.save()
    return super(CommentVoteView, self).get(request, *args, **kwargs)