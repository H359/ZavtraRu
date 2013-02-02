from peewee import *

database = PostgresqlDatabase(None)


class BaseModel(Model):
    class Meta:
        database = database

class User(BaseModel):
    date_joined = DateTimeField()
    email = CharField()
    first_name = CharField()
    is_active = BooleanField()
    is_staff = BooleanField()
    is_superuser = BooleanField()
    last_login = DateTimeField()
    last_name = CharField()
    password = CharField()
    username = CharField()

    class Meta:
        db_table = 'auth_user'


class Rubric(BaseModel):
    children_render = IntegerField()
    description = TextField()
    on_main = BooleanField()
    on_top = BooleanField()
    parent = ForeignKeyField(db_column='parent_id', rel_model='self')
    position = IntegerField()
    slug = CharField()
    title = CharField()

    class Meta:
        db_table = 'corecontent_rubric'

class Article(BaseModel):
    _base_rating = IntegerField()
    _comments_count = IntegerField()
    _rating = IntegerField()
    content = TextField()
    description = TextField()
    enabled = BooleanField()
    exclusive = BooleanField()
    kind = CharField()
    old_url = CharField()
    pub_date = DateTimeField()
    published = BooleanField()
    rubric = ForeignKeyField(db_column='rubric_id', rel_model=Rubric, null=True)
    slug = CharField()
    subtitle = CharField()
    thumbnail = CharField()
    title = CharField()

    class Meta:
        db_table = 'corecontent_contentitem'

class ArticleAuthors(BaseModel):
    contentitem = ForeignKeyField(db_column='contentitem_id', rel_model=Article, related_name='authors')
    user = ForeignKeyField(db_column='user_id', rel_model=User)

    class Meta:
        db_table = 'corecontent_contentitem_authors'

class Dailyquote(BaseModel):
    day = DateField()
    quote = TextField()
    source = ForeignKeyField(db_column='source_id', rel_model=Article)

    class Meta:
        db_table = 'corecontent_dailyquote'

class Topics(BaseModel):
    is_active = BooleanField()
    slug = CharField()
    title = CharField()

    class Meta:
        db_table = 'corecontent_featureditems'

"""
class TaggitTag(BaseModel):
    name = CharField()
    slug = CharField()

    class Meta:
        db_table = 'taggit_tag'

class TopicTag(BaseModel):
    featureditems = ForeignKeyField(db_column='featureditems_id', rel_model=CorecontentFeatureditems)
    tag = ForeignKeyField(db_column='tag_id', rel_model=TaggitTag)

    class Meta:
        db_table = 'corecontent_featureditems_tags'

class CorecontentZhivotovillustration(BaseModel):
    original = CharField()
    pub_date = DateField()
    thumbnail = CharField()
    title = CharField()

    class Meta:
        db_table = 'corecontent_zhivotovillustration'
"""