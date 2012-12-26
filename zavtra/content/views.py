from django.views.generic import DetailView

from content.models import Article, Rubric


class ArticleView(DetailView):
  template_name = 'content/article_detail.jhtml'

  def get_queryset(self):
    return Article.objects.select_related()


class RubricView(DetailView):
  template_name = 'content/rubric_detail.jhtml'

  def get_queryset(self):
    return Rubric.objects.all()
