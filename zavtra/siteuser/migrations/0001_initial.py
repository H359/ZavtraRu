# -*- coding: utf-8 -*-
import datetime
from south.db import db
from south.v2 import SchemaMigration
from django.db import models


class Migration(SchemaMigration):

    def forwards(self, orm):
        # Adding model 'User'
        db.create_table(u'siteuser_user', (
            (u'id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('password', self.gf('django.db.models.fields.CharField')(max_length=128)),
            ('last_login', self.gf('django.db.models.fields.DateTimeField')(default=datetime.datetime.now)),
            ('email', self.gf('django.db.models.fields.EmailField')(unique=True, max_length=254)),
            ('first_name', self.gf('django.db.models.fields.CharField')(max_length=250)),
            ('last_name', self.gf('django.db.models.fields.CharField')(max_length=250)),
            ('level', self.gf('django.db.models.fields.IntegerField')(default=0)),
            ('resume', self.gf('django.db.models.fields.CharField')(max_length=1024, blank=True)),
            ('bio', self.gf('django.db.models.fields.TextField')(blank=True)),
            ('photo', self.gf('django.db.models.fields.files.ImageField')(max_length=100, null=True, blank=True)),
            ('date_joined', self.gf('django.db.models.fields.DateTimeField')(default=datetime.datetime(2013, 2, 15, 0, 0))),
        ))
        db.send_create_signal(u'siteuser', ['User'])

        # Adding model 'Reader'
        db.create_table(u'siteuser_reader', (
            (u'id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('reader', self.gf('django.db.models.fields.related.ForeignKey')(related_name='readers', to=orm['siteuser.User'])),
            ('author', self.gf('django.db.models.fields.related.ForeignKey')(related_name='readees', to=orm['siteuser.User'])),
            ('subscription_start', self.gf('django.db.models.fields.DateTimeField')()),
        ))
        db.send_create_signal(u'siteuser', ['Reader'])


    def backwards(self, orm):
        # Deleting model 'User'
        db.delete_table(u'siteuser_user')

        # Deleting model 'Reader'
        db.delete_table(u'siteuser_reader')


    models = {
        u'siteuser.reader': {
            'Meta': {'object_name': 'Reader'},
            'author': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'readees'", 'to': u"orm['siteuser.User']"}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'reader': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'readers'", 'to': u"orm['siteuser.User']"}),
            'subscription_start': ('django.db.models.fields.DateTimeField', [], {})
        },
        u'siteuser.user': {
            'Meta': {'object_name': 'User'},
            'bio': ('django.db.models.fields.TextField', [], {'blank': 'True'}),
            'date_joined': ('django.db.models.fields.DateTimeField', [], {'default': 'datetime.datetime(2013, 2, 15, 0, 0)'}),
            'email': ('django.db.models.fields.EmailField', [], {'unique': 'True', 'max_length': '254'}),
            'first_name': ('django.db.models.fields.CharField', [], {'max_length': '250'}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'last_login': ('django.db.models.fields.DateTimeField', [], {'default': 'datetime.datetime.now'}),
            'last_name': ('django.db.models.fields.CharField', [], {'max_length': '250'}),
            'level': ('django.db.models.fields.IntegerField', [], {'default': '0'}),
            'password': ('django.db.models.fields.CharField', [], {'max_length': '128'}),
            'photo': ('django.db.models.fields.files.ImageField', [], {'max_length': '100', 'null': 'True', 'blank': 'True'}),
            'resume': ('django.db.models.fields.CharField', [], {'max_length': '1024', 'blank': 'True'})
        }
    }

    complete_apps = ['siteuser']