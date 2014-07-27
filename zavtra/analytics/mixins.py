from analytics.models import Hit


class HitTrackingMixin(object):
    def get_hit_object(self, obj):
        return obj['object']

    def render_to_response(self, obj):
        user = self.request.user if self.request.user.is_authenticated() else None
        Hit.objects.create(user = user, content_object = self.get_hit_object(obj))
        return super(HitTrackingMixin, self).render_to_response(obj)
