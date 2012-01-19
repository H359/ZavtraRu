#-*- coding: utf-8 -*-
# Django settings for zavtra project.
import os, socket
import pytils.translit
SITE_DIR = os.path.dirname(os.path.abspath(__file__))
DEBUG = socket.gethostname() == 'myhost'
TEMPLATE_DEBUG = DEBUG
ADMINS = (
    ('zw0rk', 'ostronom@gmail.com'),
)
MANAGERS = ADMINS
DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql_psycopg2', 
        'NAME': 'zavtra',
        'USER': 'zavtra',
        'PASSWORD': '',
        'HOST': '',
        'PORT': '',
        'OPTIONS': { 'autocommit': True },
    }
}
TIME_ZONE = 'Europe/Moscow'
LANGUAGE_CODE = 'ru-RU'
SITE_ID = 1
USE_I18N = True
USE_L10N = True
MEDIA_ROOT = os.path.join(SITE_DIR, 'media')
MEDIA_URL = '/media/'
STATIC_ROOT = os.path.join(SITE_DIR, 'static')
STATIC_URL = '/static/'
ADMIN_MEDIA_PREFIX = '/static/admin/'
STATICFILES_DIRS = ()
STATICFILES_FINDERS = (
    'django.contrib.staticfiles.finders.FileSystemFinder',
    'django.contrib.staticfiles.finders.AppDirectoriesFinder',
)
SECRET_KEY = 'ppkh78v9p3s$+5_p3+u3bdm@js&2!i$r9uy5&hg-p4$b0(yr&s'
TEMPLATE_LOADERS = (
    'django.template.loaders.filesystem.Loader',
    'django.template.loaders.app_directories.Loader',
)
if not DEBUG:
    TEMPLATE_LOADERS = (('django.template.loaders.cached.Loader', TEMPLATE_LOADERS),)
AUTHENTICATION_BACKENDS = (
    'social_auth.backends.twitter.TwitterBackend',
    'social_auth.backends.facebook.FacebookBackend',
    'social_auth.backends.google.GoogleOAuthBackend',
    #'social_auth.backends.google.GoogleOAuth2Backend',
    #'social_auth.backends.google.GoogleBackend',
    #'social_auth.backends.yahoo.YahooBackend',
    #'social_auth.backends.contrib.linkedin.LinkedinBackend',
    'social_auth.backends.contrib.livejournal.LiveJournalBackend',
    #'social_auth.backends.contrib.orkut.OrkutBackend',
    #'social_auth.backends.contrib.foursquare.FoursquareBackend',
    #'social_auth.backends.contrib.github.GithubBackend',
    #'social_auth.backends.contrib.dropbox.DropboxBackend',
    #'social_auth.backends.contrib.flickr.FlickrBackend',
    #'social_auth.backends.OpenIDBackend',
    'django.contrib.auth.backends.ModelBackend',
)
SOCIAL_AUTH_ENABLED_BACKENDS = ('google-oauth', 'openid', 'livejournal', 'twitter', 'facebook')
LOGIN_URL          = '/login/'
LOGIN_REDIRECT_URL = '/' #logged-in/'
LOGIN_ERROR_URL    = '/' #login-error/'
#SOCIAL_AUTH_COMPLETE_URL_NAME  = 'complete'
#SOCIAL_AUTH_ASSOCIATE_URL_NAME = 'associate_complete'
SOCIAL_AUTH_USERNAME_FIXER = lambda u: pytils.translit.slugify(u)
SOCIAL_AUTH_UUID_LENGTH = 16
SOCIAL_AUTH_SESSION_EXPIRATION = False
SOCIAL_AUTH_SANITIZE_REDIRECTS = False
ACCOUNT_ACTIVATION_DAYS = 7
AUTH_PROFILE_MODULE = 'siteuser.SiteProfile'
DEFAULT_FROM_EMAIL = 'noreply@zavtra.ru'
EMAIL_HOST = 'localhost'
EMAIL_PORT = 25
if not DEBUG:
    SESSION_ENGINE = 'redis_sessions.session'
MIDDLEWARE_CLASSES = (
    'django.middleware.common.CommonMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
)
if DEBUG:
    MIDDLEWARE_CLASSES += ('debug_toolbar.middleware.DebugToolbarMiddleware',)
TEMPLATE_CONTEXT_PROCESSORS = (
    'django.core.context_processors.static',
    'django.core.context_processors.media',
    'django.contrib.auth.context_processors.auth',
    'django.core.context_processors.request',
    'zavtra.corecontent.context_processors.common_pieces',
    'social_auth.context_processors.social_auth_by_type_backends',
)
ROOT_URLCONF = 'zavtra.urls'
TEMPLATE_DIRS = (
    os.path.join(SITE_DIR, 'templates'),
)
CACHES = {
    'default': {
        'BACKEND': 'django.core.cache.backends.dummy.DummyCache',
    }
}
INSTALLED_APPS = (
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.sites',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'django.contrib.admin',
    'django.contrib.sitemaps',
    'captcha',
    'filebrowser',
    'annoying',
    'pytils',
    'autoslug',
    'corecontent',
    #'sorl.thumbnail',
    'taggit',
    'taggit_autosuggest',
    'voting',
    #'mptt',
    #'treebeard',
    'mail',
    'social_auth',
    'siteuser',
    'comments',
    'minipoll',
    'editorial',
    'pipeline',
    'haystack',
    'ajaxfields',
    'imagekit',
)
HAYSTACK_CONNECTIONS = {
    'default': {
        'ENGINE': 'haystack.backends.solr_backend.SolrEngine',
        'URL': 'http://localhost:8080/solr',
        'TIMEOUT': 60 * 5,
        'INCLUDE_SPELLING': True,
    }
}
if DEBUG:
    INSTALLED_APPS = list(INSTALLED_APPS)
    INSTALLED_APPS.append('debug_toolbar')
AUTOSLUG_SLUGIFY_FUNCTION = 'zavtra.utils.slugify'
LOGGING = {
    'version': 1,
    'disable_existing_loggers': False,
    'formatters': {
        'simple': {
            'format': '%(levelname)s %(message)s'
        },
     },    
     'handlers': {
        'mail_admins': {
            'level': 'ERROR',
            'class': 'django.utils.log.AdminEmailHandler'
        },
        'console':{
            'level':'DEBUG',
            'class':'logging.StreamHandler',
            'formatter': 'simple'
        },
     },
     'loggers': {
        'social_auth.views': {
            'handlers': ['console'],
            'level': 'INFO',
            'propagate': True,
        },
        'django.request': {
            'handlers': ['mail_admins'],
            'level': 'ERROR',
            'propagate': True,
        },
     }
}
#INTERNAL_IPS = ('127.0.0.1',)
DEBUG_TOOLBAR_CONFIG = {
    'INTERCEPT_REDIRECTS': False
}
CONVERT_FILENAME = True
FILEBROWSER_EXCLUDE = []

#PIPELINE = True
PIPELINE_AUTO = False
PIPELINE_VERSION = True
PIPELINE_VERSIONING = 'pipeline.versioning.git.GitHeadRevVersioning'
#PIPELINE_CSS_COMPRESSOR = 'pipeline.compressors.csstidy.CSSTidyCompressor'
#PIPELINE_JS_COMPRESSOR = 'pipeline.compressors.closure.ClosureCompressor'
#PIPELINE_CSSTIDY_BINARY = '/usr/bin/csstidy'
#PIPELINE_CLOSURE_BINARY = '/usr/bin/closure'
PIPELINE_YUI_BINARY = '/usr/bin/yuicompressor'
PIPELINE_CSS = {
    'main': {
	'source_filenames': ('css/*.css',),
	'output_filename': 'css/compressed_css.r2.?.css',
    }
}
PIPELINE_JS = {
    'main': {
	'source_filenames': (
	    'js/jquery.cookie.js',
	    'js/bootstrap-dropdown.js',
	    'js/bootstrap-modal.js',
	    'js/bootstrap-twipsy.js',
	    'js/bootstrap-popover.js',
	    'js/jquery.tools.min.js',
	    'js/core.js',
	    'js/comments.js'
	),
	'output_filename': 'js/compressed_js.r2.?.js',
    }
}

"""
VERSIONS = {
    'thumbnail': {'verbose_name': u'Очень малеьнкое изображение (для главной)', 'width': 60, 'height': 60, 'opts': 'crop'},
    'small': {'verbose_name': u'Маленькое изображение', 'width': 150, 'height': '', 'opts': ''},
    'medium': {'verbose_name': u'Среднее изображение', 'width': 300, 'height': '', 'opts': ''},
    'large': {'verbose_name': u'Большое изображение', 'width': 680, 'height': '', 'opts': ''},
}
"""

try:
    from local_settings import *
except ImportError:
    pass