from django.views.generic import TemplateView
from django.views.generic.edit import FormView


class RegisterView(TemplateView, FormView):
  template_name = 'siteuser/register.jhtml'

  def return_form(self, form):
    return self.render_to_response(self.get_context_data(form=form))

  def post(self, request, *args, **kwargs):
    form = RegisterUserForm(request.POST)
    if form.is_valid():
      data = form.cleaned_data
      user = User.objects.create_user(email=data['email'], password=data['password1'])
      user.first_name = data.get('first_name')
      user.last_name = data.get('last_name')
      user.save()
      return redirect('siteuser.views.register_done')
    return self.return_form(form)

  def get(self, request, *args, **kwargs):
    return self.return_form(RegisterUserForm())


class RegisterDoneView(TemplateView):
  template_name = 'siteuser/register_done.jhtml'
