from django.shortcuts import redirect, get_object_or_404

from models import Link

def expand(request, short):
    link = get_object_or_404(Link, short=short)
    link.hits += 1
    link.save()
    return redirect(link.original)