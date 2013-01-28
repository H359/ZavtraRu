#-*- coding: utf-8 -*-
from django.core.urlresolvers import reverse

from grappelli.dashboard import modules, Dashboard
from grappelli.dashboard.utils import get_admin_site_name


class CustomIndexDashboard(Dashboard):
    def init_with_context(self, context):
        site_name = get_admin_site_name(context)
        
        self.children.append(modules.Group(
            u'Управление',
            column=1,
            collapsible=True,
            children = [
                modules.AppList(
                    u'Администрирование',
                    column=1,
                    collapsible=False,
                    models=('django.contrib.*',),
                ),
                modules.AppList(
                    u'Содержимое',
                    column=1,
                    css_classes=('collapse closed',),
                    exclude=('django.contrib.*',),
                )
            ]
        ))
                
        # append another link list module for "support".
        self.children.append(modules.LinkList(
            u'Медиа-файлы',
            column=2,
            children=[
                {
                    'title': u'Файлы',
                    'url': '/admin/filebrowser/browse/',
                    'external': False,
                },
            ]
        ))