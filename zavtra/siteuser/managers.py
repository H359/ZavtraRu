from django.contrib.auth.models import AbstractBaseUser, BaseUserManager


class UserManager(BaseUserManager):
  def create_user(self, email, password=None):
    if not email:
      raise ValueError(u'User must have email address')
    user = self.model(email=UserManager.normalize_email(email))
    user.set_password(password)
    user.level = self.model.USER_LEVELS.ordinary
    user.save(using=self._db)
    return user

  def create_superuser(self, email, password):
    user = self.create_user(email, password=password)
    user.level = self.model.USER_LEVELS.staff
    user.save(using=self._db)
    return user


class ColumnistsManager(UserManager):
  def get_query_set(self):
    return super(ColumnistsManager, self).get_query_set().\
           filter(level__gte = self.model.USER_LEVELS.columnist)