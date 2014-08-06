# -*- coding: utf-8 -*-
import datetime
from south.db import db
from south.v2 import SchemaMigration
from django.db import models


class Migration(SchemaMigration):

    def forwards(self, orm):
        # Adding model 'GoldenAuthor'
        db.create_table(u'siteuser_goldenauthor', (
            (u'id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('user', self.gf('django.db.models.fields.related.ForeignKey')(related_name='golden_authors', to=orm['siteuser.User'])),
            ('position', self.gf('django.db.models.fields.PositiveSmallIntegerField')()),
        ))
        db.send_create_signal(u'siteuser', ['GoldenAuthor'])


    def backwards(self, orm):
        # Deleting model 'GoldenAuthor'
        db.delete_table(u'siteuser_goldenauthor')


    models = {
        u'siteuser.goldenauthor': {
            'Meta': {'object_name': 'GoldenAuthor'},
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'position': ('django.db.models.fields.PositiveSmallIntegerField', [], {}),
            'user': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'golden_authors'", 'to': u"orm['siteuser.User']"})
        },
        u'siteuser.reader': {
            'Meta': {'object_name': 'Reader'},
            'author': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'readees'", 'to': u"orm['siteuser.User']"}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'reader': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'readers'", 'to': u"orm['siteuser.User']"}),
            'subscription_start': ('django.db.models.fields.DateTimeField', [], {})
        },
        u'siteuser.user': {
            'Meta': {'ordering': "('last_name', 'first_name')", 'object_name': 'User'},
            'allow_login': ('django.db.models.fields.BooleanField', [], {'default': 'True'}),
            'bio': ('django.db.models.fields.TextField', [], {'blank': 'True'}),
            'date_joined': ('django.db.models.fields.DateTimeField', [], {'default': 'datetime.datetime(2014, 8, 6, 0, 0)'}),
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