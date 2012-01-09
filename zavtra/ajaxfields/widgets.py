from django import forms
from django.conf import settings
from django.utils.safestring import mark_safe
from django.core.urlresolvers import reverse
from django.utils import simplejson

class AjaxFilteringSelectMultiple(forms.SelectMultiple):
    def render_choices(self, name, choices=()):
	values = ''.join(['<option value="%d">%s</option>' % (x, y) for x,y in choices])
	attrs = {'class':'filtered'}
	if name is not None:
	    attrs['name'] = name
	return '<select %s multiple="multiple">%s</select>' % (' '.join(['%s=%s' % (x,y) for x,y in attrs.items()]), values)
    def render(self, name, value, attrs=None):
	if value:
	    selected = self.model.objects.filter(pk__in = value)
	else:
	    selected = self.model.objects.none()
	leftside = self.render_choices(None, self.choices)
	rightside = self.render_choices(name, ((x.pk, unicode(x)) for x in selected))
	return mark_safe("""
	<div id="ajax_filter_selector_for_%s" class="selector">
	    <div class="selector-available">
		<h2>&nbsp;</h2>
		<p class="selector-filter">
		    <label><img src="%simg/selector-search.gif" class="help-tooltip" alt="" /></label>
		    <input type="text" />
		</p>
		%s
	    </div>
	    <ul class="selector-chooser">
		<li><a href="#" class="selector-add">&rarr;</a></li>
		<li><a href="#" class="selector-remove">&larr;</a></li>
	    </ul>
	    <div class="selector-chosen">
		<h2>&nbsp;</h2>
		<p class="selector-fitler">
		    <label>&nbsp;</label>
		</p>
		%s
	    </div>
	</div>
	<script>ajaxFields({'name':'ajax_filter_selector_for_%s','multiple':true,'app':'%s','model':'%s','endpoint': '%s','guard':%s, 'fields':%s});</script>"""
	 % (name,
	    settings.ADMIN_MEDIA_PREFIX,
	    leftside,
	    rightside,
	    name,
	    self.model._meta.app_label,
	    self.model._meta.object_name,
	    reverse('ajaxfields.responder'),
	    simplejson.dumps(self.guard),
	    simplejson.dumps(self.fields)))

class AjaxFilteringSelect(forms.Select):
    def render(self, name, value, attrs=None):
	objs = u''.join([u'<option value="%d">%s</option>' % (x.id, unicode(x)) for x in self.model.objects.filter(pk=value)])
	output = u'<select name="%s">%s</select>' % (name, objs)
	return mark_safe(u"""
	    <div class="selector" id="ajax_filter_selector_for_%s">
	    <div class="selector-filter"><input type="text"></div>
	    %s
	    </div><script>ajaxFields({'name':'ajax_filter_selector_for_%s','multiple':false,'app':'%s','model':'%s','endpoint':'%s','guard':%s,'fields':%s});</script>
	""" % (name,
	       output,
	       name,
	       self.model._meta.app_label,
	       self.model._meta.object_name,
	       reverse('ajaxfields.responder'),
	       simplejson.dumps(self.guard),
	       simplejson.dumps(self.fields)))