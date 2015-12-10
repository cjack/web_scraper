import html,sys,os,csv,requests,json,time, re


class imageNotFound(Exception):
    pass

class imageUploadFail(Exception):
    pass

class mediaStoreUploadFail(Exception):
    pass

class venueNotFound(Exception):
    pass

class hashabledict(dict):
    def __hash__(self):
        return hash(tuple(sorted(self.items())))


work_dir = os.getcwd();
csvDir = 'childplay.csv'
img_dir = "imagesChildPlay"
os.chdir(work_dir+'/'+img_dir)
all_images = [d for d in os.listdir('.')]
all_images.sort()
os.chdir(work_dir)
# kizkaz
headers = {
"X-Parse-Application-Id": "CuDt2DYYBDpVFqTINtIaoAv2du0BywjxXq3QjNU6",
       "X-Parse-REST-API-Key": "HsEMEEYtBnoTtAkjEZADK23wJ1lWDaTdAAjy5R40",
       "Content-Type": "application/json"
}
imgHeaders = {
"X-Parse-Application-Id": "CuDt2DYYBDpVFqTINtIaoAv2du0BywjxXq3QjNU6",
"X-Parse-REST-API-Key": "HsEMEEYtBnoTtAkjEZADK23wJ1lWDaTdAAjy5R40",
'Content-Type': 'image/jpeg'
}


# staging
# headers = {
# 'X-Parse-Application-Id': 'mslr6chIzlzflTF1LzmSXCkqsPZJdvu8eJ3NIYpW',
# 'X-Parse-REST-API-Key': 'VoKHBWUFakyMKbeOQeo9kcLZRPeItwYf2x0ygcEA',
# 'Content-Type': 'application/json'
# }
# imgHeaders = {
# 'X-Parse-Application-Id': 'mslr6chIzlzflTF1LzmSXCkqsPZJdvu8eJ3NIYpW',
# 'X-Parse-REST-API-Key': 'VoKHBWUFakyMKbeOQeo9kcLZRPeItwYf2x0ygcEA',
# 'Content-Type': 'image/jpeg'
# }
# production
# headers = {
# 'X-Parse-Application-Id': 'OOZnj8GueRews4TdoPKWWUZayVSqYP0qxhE0FXDO',
# 'X-Parse-REST-API-Key': 'C4jJCKgaPwo4ktVcrVqnTlUqR8vMcWIWXL7DqyAs',
# 'Content-Type': 'application/json'
# }
# imgHeaders = {
# 'X-Parse-Application-Id': 'OOZnj8GueRews4TdoPKWWUZayVSqYP0qxhE0FXDO',
# 'X-Parse-REST-API-Key': 'C4jJCKgaPwo4ktVcrVqnTlUqR8vMcWIWXL7DqyAs',
# 'Content-Type': 'image/jpeg'
# }

def uploadToPlaceMediaStore(parseName):
    url = 'https://api.parse.com/1/classes/DealMediaStore'
    body = {
    'mediaFile':{
    'name':parseName,
    '__type':'File'
    },
    'mediaType':'photo',
    'thumbFile':{
    'name':parseName,
    '__type':'File'
    }
    }
    body = json.dumps(body)
    r = requests.post(url, data = body, headers = headers)
    if r.status_code == 201:
        mediaId = r.json()['objectId']
        print('MediaStore upload successfully')
        return mediaId
    else:
        print(r.text)
        raise mediaStoreUploadFail('MediaStore Upload Failed!')

def uploadImg(imageName):
    url = 'https://api.parse.com/1/files/'

    image = open(img_dir + '/' + imageName, 'rb')
    binary = image.read()
    image.close()

    r = requests.post(url + imageName, data=binary, headers = imgHeaders)
    if r.status_code == 201:
        parseName = r.json()['name']
        print('Upload ' + parseName + ' successfully')
        mediaId = uploadToPlaceMediaStore(parseName)
        return mediaId
    else:
        raise imageUploadFail('Failed:' + str(r.status_code) + ' ' + r.text)


def importToParse(element):
    # Staging
    dealCategory = 'vhxacQgurm'
    dealSupplier = 'childplay'
    # Production

    body ={}
    sourceURL = element[0]
    dealName = html.unescape(element[1])
    price = float(element[2].split('$')[1])
    condition = element[3]
    availability = html.unescape(element[4]).split(',')
    dealBrand = html.unescape(element[5])
    dealSubCategory = html.unescape(element[6])
    rawId = element[8]
    dealDescription = html.unescape(element[9])
    tags = html.unescape(element[10]).split(',')
    tags.remove('Home')
    dealAssetsSource = element[11].split(',')

    dealAssets = []
    for image in dealAssetsSource:
        if image in all_images:
            dealAssets.append(uploadImg(image))
        else:
            raise imageNotFound(image + ' is not found')

    body = {
    'availability':availability,
    'condition':condition,
    'dealAssets':dealAssets,
    'dealBrand':dealBrand,
    'dealCategory':dealCategory,
    'dealDescription':dealDescription,
    'dealName':dealName,
    'price':price,
    'rawId':rawId,
    'tags':tags,
    'dealSupplier':dealSupplier,
    'sourceURL':sourceURL,
    'dealSubCategory':dealSubCategory
    }
    if element[7] is not "":
        body['ageGroup'] = element[7]
    # # # import
    url = 'https://api.parse.com/1/classes/Deal'
    body = json.dumps(body)
    r = requests.post(url, data = body, headers = headers)
    print(r.text)

if __name__ == "__main__":
    with open(csvDir) as csvfile:
        spamreader = csv.reader(csvfile, delimiter = ',', quotechar = '"')
        for idx, row in enumerate(spamreader):
            if idx > 518:
                print('Start '+ str(idx))
                timeout = True
                while timeout:
                    try:
                        importToParse(row)
                        timeout = False
                    except ConnectionResetError:
                        print('reconnect after 200 seconds')
                        time.sleep(200)
                    except (imageUploadFail,mediaStoreUploadFail) as error:
                        print(error)
                        print('Sleep 20 seconds, then restart ' + str(idx+1) + ' row')
                        time.sleep(20)
                    except (imageNotFound,venueNotFound) as error:
                        print(error + ' in ' + str(idx+1) + ' rows')
                        timeout = False
                    except ValueError:
                        timeout = False
