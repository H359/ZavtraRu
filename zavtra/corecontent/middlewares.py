from django.db import connections

class SQLMiddleware(object):
    def process_response(self, request, response):
	conn = connections.all()[0]
	for q in conn.queries:
	    print q['sql'], q['time']
	    print
	return response