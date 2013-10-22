#-*- coding: utf-8 -*-
from django.conf import settings
from haystack.backends.elasticsearch_backend import ElasticsearchSearchBackend, ElasticsearchSearchEngine


class CustomElasticBackend(ElasticsearchSearchBackend):
  DEFAULT_SETTINGS = {
    'settings': {
      'analysis': {
        'analyzer': {
          'ru': {
            'type': 'custom',
            'tokenizer': 'standard',
            'filter': ['lowercase', 'russian_morphology', 'english_morphology', 'ru_stopwords', 'ru_stemming'],
          }
        },
        'filter': {
          'ru_stopwords': {
            'type': 'stop',
            'stopwords': u'а,без,более,бы,был,была,были,было,быть,в,вам,вас,весь,во,вот,все,всего,всех,вы,где,да,даже,для,до,его,ее,если,есть,еще,же,за,здесь,и,из,или,им,их,к,как,ко,когда,кто,ли,либо,мне,может,мы,на,надо,наш,не,него,нее,нет,ни,них,но,ну,о,об,однако,он,она,они,оно,от,очень,по,под,при,с,со,так,также,такой,там,те,тем,то,того,тоже,той,только,том,ты,у,уже,хотя,чего,чей,чем,что,чтобы,чье,чья,эта,эти,это,я,a,an,and,are,as,at,be,but,by,for,if,in,into,is,it,no,not,of,on,or,such,that,the,their,then,there,these,they,this,to,was,will,with'
          },
          'ru_stemming': {
            'type': 'snowball',
            'language': 'Russian'
          }
        }
      }
    }
  }


class CustomElasticSearchEngine(ElasticsearchSearchEngine):
    backend = CustomElasticBackend