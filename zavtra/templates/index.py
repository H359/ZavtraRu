#!/usr/bin/env python
# -*- coding: utf-8 -*-

# template imports
import templates.base

import spitfire.runtime
import spitfire.runtime.template
from spitfire.runtime.udn import resolve_placeholder
from spitfire.runtime.udn import resolve_udn
from spitfire.runtime.template import template_method

class index(templates.base.base):
  @template_method
  def header(self):
    _buffer = self.new_buffer()
    _buffer_write = _buffer.write
    _globals = globals()
    _self_filter_function = self.filter_function
    _buffer_write(u' ')
    _rudn_1090538219073021759 = resolve_udn(super(index, self), 'header')
    _fph5C6A8EEEC0A30CF9 = _self_filter_function(_rudn_1090538219073021759(), _rudn_1090538219073021759)
    _buffer_write(_fph5C6A8EEEC0A30CF9)
    _buffer_write(u' <div class="gsep"></div> ')
    return _buffer.getvalue()
  
  @template_method
  def content(self):
    _buffer = self.new_buffer()
    _buffer_write = _buffer.write
    _globals = globals()
    _self_filter_function = self.filter_function
    _buffer_write(u' TEST ')
    return _buffer.getvalue()
  


if __name__ == '__main__':
  import spitfire.runtime.runner
  spitfire.runtime.runner.run_template(index)

