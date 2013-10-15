#!/usr/bin/env python
# -*- coding: utf-8 -*-

import spitfire.runtime
import spitfire.runtime.template
from spitfire.runtime.udn import resolve_placeholder
from spitfire.runtime.udn import resolve_udn
from spitfire.runtime.template import template_method

class base(spitfire.runtime.template.SpitfireTemplate):
  @template_method
  def header(self):
    _buffer = self.new_buffer()
    _buffer_write = _buffer.write
    _globals = globals()
    _self_filter_function = self.filter_function
    _buffer_write(u' <nav> <ul> <li><a href="#">\u0410\u0440\u0445\u0438\u0432 &laquo;\u0417\u0430\u0432\u0442\u0440\u0430&raquo;</a></li> <li><a href="http://dentv.ru">\u0414\u0435\u043d\u044c-\u0422\u0412</a></li> <li><a href="http://denlit.at.ua">\u0414\u0435\u043d\u044c \u043b\u0438\u0442\u0435\u0440\u0430\u0442\u0443\u0440\u044b</a></li> </ul> <ul> ')
    _rph_request = resolve_placeholder('request', template=self, global_vars=_globals)
    _rudn_678748992340453171 = resolve_udn(resolve_udn(_rph_request, 'user'), 'is_authenticated')
    if _rudn_678748992340453171():
      _buffer_write(u' <li><a href="#">\u041b\u0438\u0447\u043d\u044b\u0439 \u043a\u0430\u0431\u0438\u043d\u0435\u0442</a></li> <li><a href="#">\u0412\u044b\u0445\u043e\u0434</a></li> ')
    else:
      _buffer_write(u' <li><a href="#">\u0420\u0435\u0433\u0438\u0441\u0442\u0440\u0430\u0446\u0438\u044f</a></li> <li><a href="#">\u0412\u0445\u043e\u0434</a></li> ')
    _buffer_write(u' </ul> <a href="/" id="logo"></a> </nav> ')
    return _buffer.getvalue()
  
  @template_method
  def content(self):
    _buffer = self.new_buffer()
    _buffer_write = _buffer.write
    _globals = globals()
    _self_filter_function = self.filter_function
    return _buffer.getvalue()
  
  @template_method
  def main(self):
    _buffer = self.new_buffer()
    _buffer_write = _buffer.write
    _globals = globals()
    _self_filter_function = self.filter_function
    _buffer_write(u'<!doctype html> <html> <head> <title>\u0413\u0430\u0437\u0435\u0442\u0430 &laquo;\u0417\u0430\u0432\u0442\u0440\u0430&raquo;')
    _rph_title = resolve_placeholder('title', template=self, global_vars=_globals)
    if _rph_title:
      _buffer_write(u' &mdash; ')
      _fph7FF904AA29CB2CA = _self_filter_function(_rph_title)
      _buffer_write(_fph7FF904AA29CB2CA)
    _buffer_write(u'</title> <link href="')
    _rph_STATIC_URL = resolve_placeholder('STATIC_URL', template=self, global_vars=_globals)
    _fph618557B3585231A9 = _self_filter_function(_rph_STATIC_URL)
    _buffer_write(_fph618557B3585231A9)
    _buffer_write(u'css/main.css" type="text/css" rel="stylesheet"> </head> <body> <header> ')
    _buffer_write(self.header())
    _buffer_write(u' </header> <section> ')
    _buffer_write(self.content())
    _buffer_write(u' </section> </body> </html>')
    return _buffer.getvalue()


if __name__ == '__main__':
  import spitfire.runtime.runner
  spitfire.runtime.runner.run_template(base)

