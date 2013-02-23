# -*- coding: utf-8 -*-
import datetime
from south.db import db
from south.v2 import SchemaMigration
from django.db import models


class Migration(SchemaMigration):

    def forwards(self, orm):
        # Adding field 'Article.rating'
        db.add_column(u'content_article', 'rating',
                      self.gf('django.db.models.fields.IntegerField')(default=0),
                      keep_default=False)


    def backwards(self, orm):
        # Deleting field 'Article.rating'
        db.delete_column(u'content_article', 'rating')


    models = {
        u'content.article': {
            'Meta': {'ordering': "['-published_at']", 'object_name': 'Article'},
            'announce': ('django.db.models.fields.TextField', [], {}),
            'authors': ('django.db.models.fields.related.ManyToManyField', [], {'symmetrical': 'False', 'related_name': "'articles'", 'blank': 'True', 'to': u"orm['siteuser.User']"}),
            'comments_count': ('django.db.models.fields.PositiveIntegerField', [], {'default': '0'}),
            'content': ('django.db.models.fields.TextField', [], {'default': "''"}),
            'cover_source': ('django.db.models.fields.files.ImageField', [], {'max_length': '100', 'blank': 'True'}),
            'gazetted': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'published_at': ('django.db.models.fields.DateTimeField', [], {'default': 'datetime.datetime(2013, 2, 21, 0, 0)'}),
            'rating': ('django.db.models.fields.IntegerField', [], {'default': '0'}),
            'rubric': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'articles'", 'to': u"orm['content.Rubric']"}),
            'selected_at': ('django.db.models.fields.DateTimeField', [], {'null': 'True', 'blank': 'True'}),
            'slug': ('autoslug.fields.AutoSlugField', [], {'unique': 'True', 'max_length': '1024', 'populate_from': 'None', 'unique_with': '()'}),
            'status': ('django.db.models.fields.CharField', [], {'default': "'draft'", 'max_length': '20'}),
            'subtitle': ('django.db.models.fields.CharField', [], {'max_length': '1024', 'blank': 'True'}),
            'title': ('django.db.models.fields.CharField', [], {'max_length': '1024'}),
            'topics': ('django.db.models.fields.related.ManyToManyField', [], {'symmetrical': 'False', 'related_name': "'articles'", 'blank': 'True', 'to': u"orm['content.Topic']"}),
            'type': ('django.db.models.fields.CharField', [], {'default': "'text'", 'max_length': '20'}),
            'views_count': ('django.db.models.fields.PositiveIntegerField', [], {'default': '0'})
        },
        u'content.dailyquote': {
            'Meta': {'ordering': "['-day']", 'object_name': 'DailyQuote'},
            'day': ('django.db.models.fields.DateField', [], {'default': 'datetime.datetime(2013, 2, 21, 0, 0)', 'unique': 'True'}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'quote': ('django.db.models.fields.TextField', [], {}),
            'source': ('django.db.models.fields.related.ForeignKey', [], {'to': u"orm['content.Article']"})
        },
        u'content.expertcomment': {
            'Meta': {'ordering': "['-position']", 'object_name': 'ExpertComment'},
            'article': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'expert_comments'", 'to': u"orm['content.Article']"}),
            'comment': ('django.db.models.fields.TextField', [], {}),
            'expert': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'expert_comments'", 'to': u"orm['siteuser.User']"}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'position': ('django.db.models.fields.PositiveIntegerField', [], {})
        },
        u'content.issue': {
            'Meta': {'ordering': "['-published_at']", 'object_name': 'Issue'},
            'absolute_number': ('django.db.models.fields.PositiveIntegerField', [], {}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'illustration': ('django.db.models.fields.files.ImageField', [], {'max_length': '100'}),
            'published_at': ('django.db.models.fields.DateField', [], {}),
            'relative_number': ('django.db.models.fields.PositiveIntegerField', [], {})
        },
        u'content.rubric': {
            'Meta': {'object_name': 'Rubric'},
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'in_rubricator': ('django.db.models.fields.BooleanField', [], {'default': 'True'}),
            'slug': ('autoslug.fields.AutoSlugField', [], {'unique': 'True', 'max_length': '1024', 'populate_from': "'title'", 'unique_with': '()'}),
            'title': ('django.db.models.fields.CharField', [], {'max_length': '1024'})
        },
        u'content.rubricinissue': {
            'Meta': {'ordering': "['position']", 'object_name': 'RubricInIssue'},
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'issue': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'issue_rubrics'", 'to': u"orm['content.Issue']"}),
            'position': ('django.db.models.fields.PositiveIntegerField', [], {}),
            'rubric': ('django.db.models.fields.related.ForeignKey', [], {'to': u"orm['content.Rubric']"})
        },
        u'content.topic': {
            'Meta': {'ordering': "['position']", 'object_name': 'Topic'},
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'on_top': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'position': ('django.db.models.fields.PositiveIntegerField', [], {'default': '0'}),
            'slug': ('autoslug.fields.AutoSlugField', [], {'unique': 'True', 'max_length': '1024', 'populate_from': "'title'", 'unique_with': '()'}),
            'title': ('django.db.models.fields.CharField', [], {'max_length': '1024'})
        },
        u'content.wodcite': {
            'Meta': {'object_name': 'WodCite'},
            'article': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'cites'", 'to': u"orm['content.Article']"}),
            'cite': ('django.db.models.fields.TextField', [], {}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'source': ('django.db.models.fields.CharField', [], {'max_length': '1024'}),
            'word': ('django.db.models.fields.CharField', [], {'max_length': '256'})
        },
        u'siteuser.user': {
            'Meta': {'ordering': "('last_name', 'first_name')", 'object_name': 'User'},
            'bio': ('django.db.models.fields.TextField', [], {'blank': 'True'}),
            'date_joined': ('django.db.models.fields.DateTimeField', [], {'default': 'datetime.datetime(2013, 2, 21, 0, 0)'}),
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

    complete_apps = ['content']