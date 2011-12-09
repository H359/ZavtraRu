# -*- coding: utf-8 -*-
from registration.forms import RegistrationFormNoFreeEmail
from captcha.fields import ReCaptchaField

class ReCaptchaForm(RegistrationFormNoFreeEmail):
    bad_domains = ['email.com', '20minutemail.com']
    captcha = ReCaptchaField(label=u'Проверим вашу человечность', attrs={'theme':'clean'}, help_text=u'Введите слова с картинки')