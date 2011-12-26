# -*- coding: utf-8 -*-
import operator

from django.conf import settings
from django import forms
from django.utils.safestring import mark_safe
from django.utils.translation import ugettext as _

from ajax_filtered_fields import utils

def _renderFilter(js_method_name, element_id, model, lookup_list, 
    select_related):
    """Return the html output of a filter link."""
    label, lookup_dict = lookup_list
    script = "ajax_filtered_fields.%s('%s', '%s', '%s', '%s', '%s')" % (
        js_method_name,
        element_id,
        model._meta.app_label, 
        model._meta.object_name, 
        utils.lookupToString(lookup_dict),
        select_related)
    return u"""
        <a class="ajax_filter_choice" 
            href="javascript:void(0)"
            onclick="%s">%s</a>
    """ % (script, label)


class FilteredSelectMultiple(forms.SelectMultiple):
            
    def render(self, name, value, attrs=None, choices=()):
        self._element_id = attrs['id']
        # choices links
        # if there is only one choice, then nothing will be rendered
        lookups_output = ""
        lookups = utils.getLookups(self.lookups)
        if len(lookups) > 1:
            js_method_name = "getManyToManyJSON"
            lookups_output = "\n".join(
                _renderFilter(js_method_name, self._element_id, 
                    self.model, i, self.select_related) 
                for i in lookups)
                
        # normal widget output from the anchestor
        self.choices = self._getAllChoices(value)                
        parent_output = super(FilteredSelectMultiple, self
            ).render(name, value, attrs, choices)
        
        # create the output including the django admin's Javascript code that
        # mutates the select widget into a selectfilter one
        # this assumes that /admin/jsi18n/, core.js, SelectBox.js and
        # SelectFilter2.js are loaded from the page
        verbose_name = self.model._meta.verbose_name_plural.replace('"', '\\"')
        
        output = u"""
            <div>
                %s
            </div>
            %s
            <script type="text/javascript">
                $(document).ready(function(){
                	SelectFilter.init("id_%s", "%s", 0, "%s");
                });
            </script>
        """ % (lookups_output, parent_output, name, 
            verbose_name, settings.ADMIN_MEDIA_PREFIX)
        
        return mark_safe(output)
        
    def _getAllChoices(self, value):
        value = value or []
        choices = list(self.choices)
        # convert to unicode for safe comparisong during a ValidationError
        choices_keys = [unicode(i[0]) for i in choices]
        for i in value:
            if not unicode(i) in choices_keys:
                obj = utils.getObject(self.model, {"pk": i}, self.select_related)
                choices.append((i, unicode(obj)))
        choices.sort(key=operator.itemgetter(1))
        return choices
        
        
class FilteredSelect(forms.Select):
    
    def render(self, name, value, attrs=None, choices=()):
        self._element_id = attrs['id']
        # choices links
        # if there is only one choice, then nothing will be rendered
        lookups_output = ""
        lookups = utils.getLookups(self.lookups)
        if len(lookups) > 1:
            js_method_name = "getForeignKeyJSON"
            lookups_output = "\n".join(
                _renderFilter(js_method_name, self._element_id, 
                    self.model, i, self.select_related) 
                for i in lookups)
                
        # get the selected object name
        selection = "-" * 9
        if value:
            selection = utils.getObject(self.model, {"pk": value}, 
                self.select_related)
        
        # filter selectbox input
        filter_id = "%s_input" % self._element_id
        
        # give a style to the final select widget
        _attrs = {"size": 2, "style": "width:270px;"}
        try:
            attrs.update(_attrs)
        except AttributeError:
            attrs = _attrs
            
        # normal widget output from the anchestor
        # create a field with a dummy name , the real value
        # will be retrieved from a hidden field
        parent_output = super(FilteredSelect, self
            ).render("dummy-%s" % name, value, attrs, choices)
        
        # output
        mapping = {
            "lookups_output": lookups_output,
            "selection": selection,
            "filter_id": filter_id,
            "parent_output": parent_output,
            "name": name,
            "element_id": self._element_id, 
            "value": "" if value is None else value,
            "admin_media_prefix": settings.ADMIN_MEDIA_PREFIX,
            }
                            
        output = u"""
            <div class="selector">
                %(lookups_output)s
            </div>
            
            <div class="selector">
                <div class="selector-available">
                    <h2>%(selection)s</h2>
                    <p class="selector-filter">
                        <img src="%(admin_media_prefix)simg/admin/selector-search.gif"> 
                        <input id="%(filter_id)s" type="text">
                    </p>
                    %(parent_output)s
                </div>
            </div>
            
            <input type="hidden" name="%(name)s" id="hidden-%(element_id)s" value="%(value)s" />
            
            <script type="text/javascript" charset="utf-8">
        		$(document).ready(function(){
                    SelectBox.init('%(element_id)s');

                    $("#%(filter_id)s").bind("keyup", function(e) {
                        SelectBox.filter("%(element_id)s", $("#%(filter_id)s").val())
                    });
                    
                    $(".ajax_letter").click(function(e) {
                        $("#%(filter_id)s").val("");
                    });
                    
                    ajax_filtered_fields.bindForeignKeyOptions("%(element_id)s");
        		});
        	</script>
            """ % mapping
            
        return mark_safe(output)
        