# -*- coding: utf-8 -*-
from django import forms
from django.forms.util import ValidationError
from django.utils.translation import ugettext as _

from ajax_filtered_fields.forms import FilteredSelectMultiple, FilteredSelect
from ajax_filtered_fields import utils

class AjaxManyToManyField(forms.ModelMultipleChoiceField):
    """
    Base many to many form field that display filter choices using
    JQuery ajax requests.
    """
    def __init__(self, model, lookups, default_index=0, select_related=None,
        widget=FilteredSelectMultiple, *args, **kwargs):
        """
        model: the related model
        lookups: a sequence of (label, lookup_dict) that tells how to
            filter the objects
            e.g. (
                    ('active', {'is_active': True}),
                    ('inactive', {'is_active': False}),
                    )
            you may specify what you want in lookup_dict, give multiple
            filter lookups for the same choice and also set a choice that
            gets all unfiltered objects
            e.g. (
                    ('some stuff', {
                        'field1__startswith': 'a',
                        'field2': 'value'
                        }),
                    ('all stuff', {}),
                    )

        default_index: the index of the lookup sequence that will
            be the default choice when the field is initially displayed.
            set to None if you want the widget to start empty
            
        select_related: if not None the resulting querydict is performed
            using select_related(select_related), allowing foreign keys
            to be retrieved (e.g. useful when the unicode representation 
            of the model objects contains references to foreign keys)
            
        It is possible to pass all the other args and kwargs accepted by 
        the django field class.
        """
        # get the default index and queryset
        # queryset is empty if default index is None
        if default_index is None:
            queryset = model.objects.none()
        else:
            lookups_list = utils.getLookups(lookups)
            lookup_dict = lookups_list[default_index][1]
            # get the queryset
            queryset = utils.getObjects(model, lookup_dict, select_related)
        # call the parent constructor
        super(AjaxManyToManyField, self
            ).__init__(queryset, widget=widget, *args, **kwargs)
        # populate widget with some data
        self.widget.lookups = self.lookups = lookups
        self.widget.model = self.model = model
        self.widget.select_related = select_related
        
    def clean(self, value):
        if self.required and not value:
            raise ValidationError(self.error_messages['required'])
        elif not self.required and not value:
            return []
        if not isinstance(value, (list, tuple)):
            raise ValidationError(self.error_messages['list'])
        final_values = []
        # if there is only one lookup used to limit choices, then a real
        # validation over that limited choices is performed
        lookups_list = utils.getLookups(self.lookups)
        limit_choices_to = {} if len(lookups_list) != 1 else lookups_list[0][1]
        for val in value:
            try:
                obj = self.model.objects.get(pk=val, **limit_choices_to)
            except self.model.DoesNotExist:
                raise ValidationError(self.error_messages['invalid_choice'] % val)
            else:
                final_values.append(obj)
        return final_values
        
            
class AjaxForeignKeyField(forms.ModelChoiceField):
    """
    Base foreign key form field that display filter choices using
    JQuery ajax requests.
    """
    def __init__(self, model, lookups, default_index=0, select_related=None,
        widget=FilteredSelect, *args, **kwargs):
        """
        See the AjaxManyToManyField docstring.
        """
        # get the default index and queryset
        # queryset is empty if default index is None
        if default_index is None:
            queryset = model.objects.none()
        else:
            lookups_list = utils.getLookups(lookups)
            lookup_dict = lookups_list[default_index][1]
            # get the queryset
            queryset = utils.getObjects(model, lookup_dict, select_related)
        # call the parent constructor
        super(AjaxForeignKeyField, self
            ).__init__(queryset, widget=widget, *args, **kwargs)
        # populate widget with some data
        self.widget.lookups = self.lookups = lookups
        self.widget.model = self.model = model
        self.widget.select_related = select_related
        
    def clean(self, value):
        if value in forms.fields.EMPTY_VALUES:
            return None
        # if there is only one lookup used to limit choices, then a real
        # validation over that limited choices is performed
        lookups_list = utils.getLookups(self.lookups)
        limit_choices_to = {} if len(lookups_list) != 1 else lookups_list[0][1]
        try:
            key = self.to_field_name or 'pk'
            limit_choices_to[key] = value
            value = self.model.objects.get(**limit_choices_to)
        except self.model.DoesNotExist:
            raise ValidationError(self.error_messages['invalid_choice'])
        return value


def _byLetterFactory(parent):
    """
    Factory function returning a ManyToMany or ForeignKey field with 
    filters based on initials of a field of the objects.
    parent can be AjaxManyToManyField or AjaxForeignKeyField.
    """
    class ByLetter(parent):
        """
        Ajax filtered field that displays filters based on 
        initials of a field of the objects, as they are typed by the user.
        """
        def __init__(self, model, field_name="name", *args, **kwargs):
            """
            model: the related model
            field_name: the name of the field looked up 
                for initial
        
            It is possible to pass all the other args and kwargs accepted by 
            parent ajax filtered field.
            """
            import string
            lookup_key = "%s__istartswith" % field_name
            lookups = [(i, {lookup_key: i}) for i in string.lowercase]
            # other non-letter records
            regex_lookup_key = "%s__iregex" % field_name
            lookups.append((_('other'), {regex_lookup_key: "^[^a-z]"}))
        
            super(ByLetter, self).__init__(model, lookups, *args, **kwargs)
    return ByLetter
    
ManyToManyByLetter = _byLetterFactory(AjaxManyToManyField)
ForeignKeyByLetter = _byLetterFactory(AjaxForeignKeyField)


def _byStatusFactory(parent):
    """
    Factory function returning a ManyToMany or ForeignKey field with 
    filters based on activation status of the object.
    parent can be AjaxManyToManyField or AjaxForeignKeyField.
    """
    class ByStatus(parent):
        """
        Ajax filtered field that displays filters based on 
        activation status of the objects.
        """
        def __init__(self, model, field_name="is_active", *args, **kwargs):
            """
            model: the related model
            field_name: the name of the field that
                manages the activation of the object

            It is possible to pass all the other args and kwargs accepted by 
            parent ajax filtered field.
            """
            lookups = (
                (_('active'), {field_name: True}),
                (_('inactive'), {field_name: False}),
                (_('all'), {}),
                )
            super(ByStatus, self).__init__(model, lookups, *args, **kwargs)
    return ByStatus
    
ManyToManyByStatus = _byStatusFactory(AjaxManyToManyField)
ForeignKeyByStatus = _byStatusFactory(AjaxForeignKeyField)


def _byRelatedFieldFactory(parent):
    """
    Factory function returning a ManyToMany or ForeignKey field with 
    filters based on a related field (foreign key or many to many) of the 
    object. parent can be AjaxManyToManyField or AjaxForeignKeyField.
    """
    class ByRelatedField(parent):
        """
        Ajax filtered field that displays filters based on a related field 
        (foreign key or many to many) of the object.
        """
        def __init__(self, model, field_name, include_blank=False, 
            *args, **kwargs):
            """
            model: the related model
            field_name: the name of the field representing the relationship
                between the model and the related model
            include_blank: if not False is displayed a NULL choice for
                objects without relation (field_name__isnull=True).
                The label of the choice must be specified as string.

            It is possible to pass all the other args and kwargs accepted by 
            parent ajax filtered field.
            """
            field = model._meta.get_field(field_name)
            attname = "%s__pk" % field_name

            def lookups():
                """
                Return the lookups dict. This is needed because the lookups
                may change as consequence of database changes at runtime.
                """
                choices = field.get_choices(include_blank=include_blank)
                lookups_ = [(label, {attname: pk}) for pk, label in choices if pk]
                # add the blank choice lookup
                if include_blank:
                    attname_isnull = "%s__isnull" % field_name
                    lookups_.append((include_blank, {attname_isnull: True}))
                # add the all objects lookup
                lookups_.append((_('all'), {}))
                return lookups_

            super(ByRelatedField, self).__init__(model, lookups, *args, **kwargs)
    return ByRelatedField
    
ManyToManyByRelatedField = _byRelatedFieldFactory(AjaxManyToManyField)
ForeignKeyByRelatedField = _byRelatedFieldFactory(AjaxForeignKeyField)
