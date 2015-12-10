import json, httplib, random, string, urllib
from random import randint


connection = httplib.HTTPSConnection('api.parse.com', 443)
connection.connect()

header = {
	       "X-Parse-Application-Id": "CuDt2DYYBDpVFqTINtIaoAv2du0BywjxXq3QjNU6",
	       "X-Parse-REST-API-Key": "HsEMEEYtBnoTtAkjEZADK23wJ1lWDaTdAAjy5R40"
	     }

#-------------------------------------------------------------------------------



# random length random letter mix with digits
def id_generator(chars=string.ascii_uppercase + string.digits):
    return ''.join(random.choice(chars) for _ in range(randint(4, 12)))


#-------------------------------------------------------------------------------


def insert_data(Name, Score):
	connection.request('POST', '/1/classes/GameScore', json.dumps({
       "score": Score,
       "playerName": Name,
       "cheatMode": False
     }), header)
	results = json.loads(connection.getresponse().read())
	print results


#-------------------------------------------------------------------------------

def random_add_data():

	random_times = randint(2,15)

	print "random_times is: " + str(random_times)

	while(random_times > 0):
		random_times = random_times - 1
		randomScore = randint(1000, 9999)
		randomName = id_generator()
		print "Creating Objects"
		connection.request('POST', '/1/classes/GameScore', json.dumps({
		       "score": randomScore,
		       "playerName": randomName,
		       "cheatMode": False
		     }), header)
		results = json.loads(connection.getresponse().read())
		print results



#-------------------------------------------------------------------------------

def retriving_objects():

	print "Retrieving Objects"
	connection.request('GET', '/1/classes/GameScore/yjnqtWwl2U', '', header)
	result = json.loads(connection.getresponse().read())
	print result



#-------------------------------------------------------------------------------

def query_objects():

	print "Checking the database"
	connection.request('GET', '/1/classes/GameScore', '', header)
	result = json.loads(connection.getresponse().read())
	print result
	return result

# what I am about to do is to add four new records, two of which have already 
# existed in the database, therefore need to update, while two others need to 
# add as the new record and print out the user names.

# 1. add two old records, say, (aaa, 100), (bbb, 200)
# 2. add four new record, two of which need to be updated, and others need 
#    to be added 
# 3. 
def update_data(Id, Score):
	connection.request('PUT', '/1/classes/GameScore/' + str(Id), json.dumps({
       "score": Score
     }), header)
	result = json.loads(connection.getresponse().read())
	print result


def search(Name):
	connection = httplib.HTTPSConnection('api.parse.com', 443)
	params = urllib.urlencode({"where":json.dumps({
	       "playerName": Name,
	       "cheatMode": False
	     })})
	connection.connect()
	connection.request('GET', '/1/classes/GameScore?%s' % params, '', header)
	result = json.loads(connection.getresponse().read())
	#objectId
	# print (result[u'objectId'])
	# print type(result[u'results'])
	return result
	return 


def check_data(Name, Score):
	result = search(Name)
	# print result[u'results'] == []
	if(result[u'results'] == []):
		print "add data"
		insert_data(Name, Score)
	else:
		print "update data"
		Id = ((result[u'results'])[0])[u'objectId']
		update_data(Id, Score)
		





def test_function():
	# add two old records
	insert_data("aaa", 100)
	insert_data("bbb", 200)
	# add four newed records
	check_data("aaa", 111)
	check_data("bbb", 222)
	check_data("ccc", 333)
	check_data("ddd", 444)



#-------------------------------------------------------------------------------

if __name__ == '__main__':
	# random_add_data()
	# query_objects()
	#MGELRWZ
	# print search("MGELRWZ1")
	# check_data("MGELRWZ112", 1111)
	# test_function()
	test_function()

	# connection = httplib.HTTPSConnection('api.parse.com', 443)
	# params = urllib.urlencode({"where":json.dumps({
	#        "playerName": "MGELRWZ1",
	#        "cheatMode": False
	#      })})
	# connection.connect()
	# connection.request('GET', '/1/classes/GameScore?%s' % params, '', header)
	# result = json.loads(connection.getresponse().read())
	# print (result[u'results'] is [])
	print "Done"



#-------------------------------------------------------------------------------