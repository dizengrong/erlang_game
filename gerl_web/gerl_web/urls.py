# -*- coding: utf8 -*-
from django.conf.urls import patterns, include, url
import xadmin
xadmin.autodiscover()

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

# from xadmin.plugins import xversion
# xversion.registe_models()

urlpatterns = patterns('',
    # Examples:
    # url(r'^$', 'gerl_web.views.home', name='home'),
    # url(r'^gerl_web/', include('gerl_web.foo.urls')),

    # Uncomment the admin/doc line below to enable admin documentation:
    # url(r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the admin:
    url(r'^admin/', include(admin.site.urls)),
    url(r'^', include(xadmin.site.urls)),
)
