# -*- coding: utf-8 -*-

# request helpers

def _cleanValue(value):
    mapping = {
        "True": True,
        "False": False,
        "None": None,
        }
    return mapping.get(value, value)

def lookupToString(lookup_dict):
    """
    Convert the lookup dict into a string.
    e.g.:
        {"field1": "a", "field2": "b"} -> "field1=a,field2=b"
    """
    return ",".join("%s=%s" % (k, v) for k, v in lookup_dict.items())

def stringToLookup(lookup_string):
    """
    Convert the lookup string into a dict.
    e.g.:
        "field1=a,field2=b" -> {"field1": "a", "field2": "b"}
    """
    lookup_dict = {}
    for i in lookup_string.split(","):
        if i:
            key, value = i.split("=")
            lookup_dict[str(key)] = _cleanValue(value)
    return lookup_dict
    
# query helpers

def getLookups(lookups):
    """
    The given "lookups" can be a callable object or a sequence.
    Return it as a list.
    """
    return list(lookups() if callable(lookups) else lookups)

def _getManager(model, select_related):
    if select_related is not None:
        return model.objects.select_related(select_related)
    return model.objects
    
def getObjects(model, lookup_dict, select_related):
    """
    Return the queryset given the model, the lookup dict and
    the select_related part of the queryset (ignored if None).
    """
    manager = _getManager(model, select_related)
    return manager.filter(**lookup_dict)
    
def getObject(model, lookup_dict, select_related):
    """
    Return the model instance given the model, the lookup dict and
    the select_related part of the queryset (ignored if None).
    """
    manager = _getManager(model, select_related)
    return manager.get(**lookup_dict)
