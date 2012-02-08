from django.db import models

Alphabet = map(chr, range(48,58) + range(65,91) + range(97,123))

class Link(models.Model):
    short    = models.CharField(max_length=1024)
    original = models.CharField(max_length=255)
    hits     = models.IntegerField(default=0)

    def shorten(self):
	res = []
	alen = len(Alphabet)
	try:
	    l = Link.objects.order_by('-id')[0]
	    last_id = l.id
	except IndexError:
	    last_id = 0
	signs, last = divmod(last_id, alen)
	res = [Alphabet[-1] for _ in range(signs)]
	res.append(Alphabet[last])
	return ''.join(res)

    def save(self, *args, **kwargs):
	if self.short is None or len(self.short) == 0:
	    self.short = self.shorten()
	super(Link, self).save(*args, **kwargs)

    @models.permalink
    def get_absolute_url(self):
	return ('shortener.expand', (), {'short': self.short})