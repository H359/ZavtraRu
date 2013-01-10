from django.shortcuts import redirect

from comments.forms import CommentForm


def add_comment(request):
  if request.method == 'POST':
    form = CommentForm(request.POST)
    if form.is_valid():
      comment = form.save(commit=False)
      comment.author = request.user
      comment.save()
    return redirect(request.POST.get('next'))
