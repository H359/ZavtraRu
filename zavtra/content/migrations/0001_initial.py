# -*- coding: utf-8 -*-
import datetime
from south.db import db
from south.v2 import SchemaMigration
from django.db import models


class Migration(SchemaMigration):

    def forwards(self, orm):
        # Adding model 'Rubric'
        db.create_table(u'content_rubric', (
            (u'id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('title', self.gf('django.db.models.fields.CharField')(max_length=1024)),
            ('slug', self.gf('autoslug.fields.AutoSlugField')(unique=True, max_length=1024, populate_from='title', unique_with=())),
        ))
        db.send_create_signal(u'content', ['Rubric'])

        # Adding model 'Issue'
        db.create_table(u'content_issue', (
            (u'id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('absolute_number', self.gf('django.db.models.fields.PositiveIntegerField')()),
            ('relative_number', self.gf('django.db.models.fields.PositiveIntegerField')()),
            ('published_at', self.gf('django.db.models.fields.DateField')()),
            ('illustration', self.gf('django.db.models.fields.files.ImageField')(max_length=100)),
        ))
        db.send_create_signal(u'content', ['Issue'])

        # Adding model 'RubricInIssue'
        db.create_table(u'content_rubricinissue', (
            (u'id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('issue', self.gf('django.db.models.fields.related.ForeignKey')(related_name='issue_rubrics', to=orm['content.Issue'])),
            ('rubric', self.gf('django.db.models.fields.related.ForeignKey')(to=orm['content.Rubric'])),
            ('position', self.gf('django.db.models.fields.PositiveIntegerField')()),
        ))
        db.send_create_signal(u'content', ['RubricInIssue'])

        # Adding model 'Topic'
        db.create_table(u'content_topic', (
            (u'id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('title', self.gf('django.db.models.fields.CharField')(max_length=1024)),
            ('slug', self.gf('autoslug.fields.AutoSlugField')(unique=True, max_length=1024, populate_from='title', unique_with=())),
            ('position', self.gf('django.db.models.fields.PositiveIntegerField')(default=0)),
            ('on_top', self.gf('django.db.models.fields.BooleanField')(default=False)),
        ))
        db.send_create_signal(u'content', ['Topic'])

        # Adding model 'Article'
        db.create_table(u'content_article', (
            (u'id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('rubric', self.gf('django.db.models.fields.related.ForeignKey')(related_name='articles', to=orm['content.Rubric'])),
            ('title', self.gf('django.db.models.fields.CharField')(max_length=1024)),
            ('slug', self.gf('autoslug.fields.AutoSlugField')(unique=True, max_length=1024, populate_from=None, unique_with=())),
            ('subtitle', self.gf('django.db.models.fields.CharField')(max_length=1024, blank=True)),
            ('status', self.gf('django.db.models.fields.CharField')(default='draft', max_length=20)),
            ('type', self.gf('django.db.models.fields.CharField')(default='text', max_length=20)),
            ('published_at', self.gf('django.db.models.fields.DateTimeField')(default=datetime.datetime(2013, 2, 15, 0, 0))),
            ('selected_at', self.gf('django.db.models.fields.DateTimeField')(null=True, blank=True)),
            ('cover_source', self.gf('django.db.models.fields.files.ImageField')(max_length=100, blank=True)),
            ('announce', self.gf('django.db.models.fields.TextField')()),
            ('content', self.gf('django.db.models.fields.TextField')(default='')),
            ('gazetted', self.gf('django.db.models.fields.BooleanField')(default=False)),
            ('comments_count', self.gf('django.db.models.fields.PositiveIntegerField')(default=0)),
            ('views_count', self.gf('django.db.models.fields.PositiveIntegerField')(default=0)),
        ))
        db.send_create_signal(u'content', ['Article'])

        # Adding M2M table for field authors on 'Article'
        db.create_table(u'content_article_authors', (
            ('id', models.AutoField(verbose_name='ID', primary_key=True, auto_created=True)),
            ('article', models.ForeignKey(orm[u'content.article'], null=False)),
            ('user', models.ForeignKey(orm[u'siteuser.user'], null=False))
        ))
        db.create_unique(u'content_article_authors', ['article_id', 'user_id'])

        # Adding M2M table for field topics on 'Article'
        db.create_table(u'content_article_topics', (
            ('id', models.AutoField(verbose_name='ID', primary_key=True, auto_created=True)),
            ('article', models.ForeignKey(orm[u'content.article'], null=False)),
            ('topic', models.ForeignKey(orm[u'content.topic'], null=False))
        ))
        db.create_unique(u'content_article_topics', ['article_id', 'topic_id'])

        # Adding model 'ExpertComment'
        db.create_table(u'content_expertcomment', (
            (u'id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('expert', self.gf('django.db.models.fields.related.ForeignKey')(related_name='expert_comments', to=orm['siteuser.User'])),
            ('article', self.gf('django.db.models.fields.related.ForeignKey')(related_name='expert_comments', to=orm['content.Article'])),
            ('comment', self.gf('django.db.models.fields.TextField')()),
            ('position', self.gf('django.db.models.fields.PositiveIntegerField')()),
        ))
        db.send_create_signal(u'content', ['ExpertComment'])

        # Adding model 'DailyQuote'
        db.create_table(u'content_dailyquote', (
            (u'id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('quote', self.gf('django.db.models.fields.TextField')()),
            ('source', self.gf('django.db.models.fields.related.ForeignKey')(to=orm['content.Article'])),
            ('day', self.gf('django.db.models.fields.DateField')(default=datetime.datetime(2013, 2, 15, 0, 0), unique=True)),
        ))
        db.send_create_signal(u'content', ['DailyQuote'])


    def backwards(self, orm):
        # Deleting model 'Rubric'
        db.delete_table(u'content_rubric')

        # Deleting model 'Issue'
        db.delete_table(u'content_issue')

        # Deleting model 'RubricInIssue'
        db.delete_table(u'content_rubricinissue')

        # Deleting model 'Topic'
        db.delete_table(u'content_topic')

        # Deleting model 'Article'
        db.delete_table(u'content_article')

        # Removing M2M table for field authors on 'Article'
        db.delete_table('content_article_authors')

        # Removing M2M table for field topics on 'Article'
        db.delete_table('content_article_topics')

        # Deleting model 'ExpertComment'
        db.delete_table(u'content_expertcomment')

        # Deleting model 'DailyQuote'
        db.delete_table(u'content_dailyquote')


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
            'published_at': ('django.db.models.fields.DateTimeField', [], {'default': 'datetime.datetime(2013, 2, 15, 0, 0)'}),
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
            'day': ('django.db.models.fields.DateField', [], {'default': 'datetime.datetime(2013, 2, 15, 0, 0)', 'unique': 'True'}),
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

    complete_apps = ['content']