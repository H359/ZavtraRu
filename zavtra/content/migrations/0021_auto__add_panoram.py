# -*- coding: utf-8 -*-
import datetime
from south.db import db
from south.v2 import SchemaMigration
from django.db import models


class Migration(SchemaMigration):

    def forwards(self, orm):
        # Adding model 'Panoram'
        db.create_table(u'content_panoram', (
            (u'id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('title', self.gf('django.db.models.fields.CharField')(max_length=1024)),
            ('slug', self.gf('autoslug.fields.AutoSlugField')(unique=True, max_length=1024, populate_from='title', unique_with=())),
            ('on_main', self.gf('django.db.models.fields.BooleanField')(default=False)),
            ('position', self.gf('django.db.models.fields.PositiveIntegerField')()),
            ('image', self.gf('django.db.models.fields.files.ImageField')(max_length=100)),
        ))
        db.send_create_signal('content', ['Panoram'])

        # Adding M2M table for field topics on 'Panoram'
        db.create_table(u'content_panoram_topics', (
            ('id', models.AutoField(verbose_name='ID', primary_key=True, auto_created=True)),
            ('panoram', models.ForeignKey(orm['content.panoram'], null=False)),
            ('topic', models.ForeignKey(orm['content.topic'], null=False))
        ))
        db.create_unique(u'content_panoram_topics', ['panoram_id', 'topic_id'])


    def backwards(self, orm):
        # Deleting model 'Panoram'
        db.delete_table(u'content_panoram')

        # Removing M2M table for field topics on 'Panoram'
        db.delete_table('content_panoram_topics')


    models = {
        'content.article': {
            'Meta': {'ordering': "['-published_at']", 'object_name': 'Article'},
            '_issue': ('django.db.models.fields.IntegerField', [], {'default': '-1'}),
            'announce': ('django.db.models.fields.TextField', [], {}),
            'authors': ('django.db.models.fields.related.ManyToManyField', [], {'symmetrical': 'False', 'related_name': "'articles'", 'blank': 'True', 'to': u"orm['siteuser.User']"}),
            'comments_count': ('django.db.models.fields.PositiveIntegerField', [], {'default': '0'}),
            'content': ('django.db.models.fields.TextField', [], {'default': "''"}),
            'cover_source': ('django.db.models.fields.files.ImageField', [], {'max_length': '100', 'blank': 'True'}),
            'gazetted': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'published_at': ('django.db.models.fields.DateTimeField', [], {'default': 'datetime.datetime(2013, 9, 26, 0, 0)'}),
            'rating': ('django.db.models.fields.IntegerField', [], {'default': '0'}),
            'rubric': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'articles'", 'to': "orm['content.Rubric']"}),
            'selected_at': ('django.db.models.fields.DateTimeField', [], {'null': 'True', 'blank': 'True'}),
            'show_icon': ('django.db.models.fields.BooleanField', [], {'default': 'True'}),
            'slug': ('autoslug.fields.AutoSlugField', [], {'unique': 'True', 'max_length': '1024', 'populate_from': "'title'", 'unique_with': '()'}),
            'status': ('django.db.models.fields.CharField', [], {'default': "'draft'", 'max_length': '20'}),
            'subtitle': ('django.db.models.fields.CharField', [], {'max_length': '1024', 'blank': 'True'}),
            'title': ('django.db.models.fields.CharField', [], {'max_length': '1024'}),
            'topics': ('django.db.models.fields.related.ManyToManyField', [], {'symmetrical': 'False', 'related_name': "'articles'", 'blank': 'True', 'to': "orm['content.Topic']"}),
            'type': ('django.db.models.fields.CharField', [], {'default': "'text'", 'max_length': '20'}),
            'views_count': ('django.db.models.fields.PositiveIntegerField', [], {'default': '0'})
        },
        'content.articlevote': {
            'Meta': {'unique_together': "(('article', 'user'),)", 'object_name': 'ArticleVote'},
            'article': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'votes'", 'to': "orm['content.Article']"}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'user': ('django.db.models.fields.related.ForeignKey', [], {'to': u"orm['siteuser.User']"}),
            'vote': ('django.db.models.fields.SmallIntegerField', [], {'default': '0'})
        },
        'content.expertcomment': {
            'Meta': {'ordering': "['-position']", 'object_name': 'ExpertComment'},
            'article': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'expert_comments'", 'to': "orm['content.Article']"}),
            'comment': ('django.db.models.fields.TextField', [], {}),
            'expert': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'expert_comments'", 'to': u"orm['siteuser.User']"}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'position': ('django.db.models.fields.PositiveIntegerField', [], {})
        },
        'content.extractedquote': {
            'Meta': {'ordering': "('-created_at',)", 'object_name': 'ExtractedQuote'},
            'created_at': ('django.db.models.fields.DateTimeField', [], {'auto_now': 'True', 'blank': 'True'}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'quote': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'source': ('django.db.models.fields.related.ForeignKey', [], {'to': "orm['content.Article']"})
        },
        'content.issue': {
            'Meta': {'ordering': "['-published_at']", 'object_name': 'Issue'},
            'absolute_number': ('django.db.models.fields.PositiveIntegerField', [], {}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'illustration': ('django.db.models.fields.files.ImageField', [], {'max_length': '100'}),
            'illustration_text': ('django.db.models.fields.CharField', [], {'default': 'None', 'max_length': '128', 'null': 'True', 'blank': 'True'}),
            'published_at': ('django.db.models.fields.DateField', [], {}),
            'relative_number': ('django.db.models.fields.PositiveIntegerField', [], {})
        },
        'content.panoram': {
            'Meta': {'ordering': "('position',)", 'object_name': 'Panoram'},
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'image': ('django.db.models.fields.files.ImageField', [], {'max_length': '100'}),
            'on_main': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'position': ('django.db.models.fields.PositiveIntegerField', [], {}),
            'slug': ('autoslug.fields.AutoSlugField', [], {'unique': 'True', 'max_length': '1024', 'populate_from': "'title'", 'unique_with': '()'}),
            'title': ('django.db.models.fields.CharField', [], {'max_length': '1024'}),
            'topics': ('django.db.models.fields.related.ManyToManyField', [], {'to': "orm['content.Topic']", 'symmetrical': 'False'})
        },
        'content.rubric': {
            'Meta': {'object_name': 'Rubric'},
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'in_rubricator': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'is_public': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'slug': ('autoslug.fields.AutoSlugField', [], {'unique': 'True', 'max_length': '1024', 'populate_from': "'title'", 'unique_with': '()'}),
            'title': ('django.db.models.fields.CharField', [], {'max_length': '1024'})
        },
        'content.rubricinissue': {
            'Meta': {'ordering': "['position']", 'object_name': 'RubricInIssue'},
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'issue': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'issue_rubrics'", 'to': "orm['content.Issue']"}),
            'position': ('django.db.models.fields.PositiveIntegerField', [], {}),
            'rubric': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'issue_rubrics'", 'to': "orm['content.Rubric']"})
        },
        'content.specialproject': {
            'Meta': {'object_name': 'SpecialProject'},
            'articles': ('django.db.models.fields.related.ManyToManyField', [], {'to': "orm['content.Article']", 'symmetrical': 'False'}),
            'date': ('django.db.models.fields.DateField', [], {'default': 'datetime.datetime(2013, 9, 26, 0, 0)'}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'slug': ('autoslug.fields.AutoSlugField', [], {'unique': 'True', 'max_length': '1024', 'populate_from': "'title'", 'unique_with': '()'}),
            'title': ('django.db.models.fields.CharField', [], {'max_length': '1024'})
        },
        'content.topic': {
            'Meta': {'ordering': "['position']", 'object_name': 'Topic'},
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'on_top': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'position': ('django.db.models.fields.PositiveIntegerField', [], {'default': '0'}),
            'slug': ('autoslug.fields.AutoSlugField', [], {'unique': 'True', 'max_length': '1024', 'populate_from': "'title'", 'unique_with': '()'}),
            'title': ('django.db.models.fields.CharField', [], {'max_length': '1024'})
        },
        'content.wodcite': {
            'Meta': {'object_name': 'WodCite'},
            'article': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'cites'", 'to': "orm['content.Article']"}),
            'cite': ('django.db.models.fields.TextField', [], {}),
            u'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'source': ('django.db.models.fields.CharField', [], {'max_length': '1024'})
        },
        u'siteuser.user': {
            'Meta': {'ordering': "('last_name', 'first_name')", 'object_name': 'User'},
            'allow_login': ('django.db.models.fields.BooleanField', [], {'default': 'True'}),
            'bio': ('django.db.models.fields.TextField', [], {'blank': 'True'}),
            'date_joined': ('django.db.models.fields.DateTimeField', [], {'default': 'datetime.datetime(2013, 9, 26, 0, 0)'}),
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