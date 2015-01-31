"""-ELEPHANTS == JAGUARS-"""

import hashlib
import time
import requests
import os
import json
import base64

SERVER = "http://localhost:8000"
CACHE = {}


def pulse(path):
    """
    Perform a single pulse, synchronising data between the client and the
    server. As long as there aren't concurrent modifications, everything
    should mostly work out
    """
    request = {
        'Changed': {},
        'Unmodified': {},
        'Added': {},
    }

    # Generate the change set
    # TODO(michael): Recurse through subdirectories
    # TODO(michael): Don't crash when you hit a folder
    files = os.listdir(path)
    for f in files:
        mtime = os.stat(os.path.join(path, f)).st_mtime

        if f in CACHE:
            if CACHE[f]['mtime'] == mtime:
                request['Unmodified'][f] = {'Hash': CACHE[f]['Hash']}
                continue
            else:
                # Update the CACHE's modified time
                CACHE[f]['mtime'] = mtime
                body, hsh = get_hash(f, path)

                if CACHE[f]['Hash'] == hsh:
                    request['Unmodified'][f] = {'Hash': hsh}
                    continue
                else:
                    # Update the CACHE's hash
                    CACHE[f]['Hash'] = hsh
                    request['Changed'][f] = {'Hash': hsh, 'Data': body}
        else:
            body, hsh = get_hash(f, path)
            CACHE[f] = {
                'Hash': hsh,
                'mtime': mtime
            }
            request['Added'][f] = {'Hash': hsh, 'Data': body}

    print request
    # Perform the sync request
    request = json.dumps(request)
    res = json.loads(requests.post(SERVER+"/sync", data=request).text)

    print res

    for f, data in res['Update'].iteritems():
        print "Updating file {}".format(f)

        with open(os.path.join(path, f), "w") as handle:
            handle.write(data['Data'])
        CACHE[f] = {
            'mtime': os.stat(os.path.join(path, f)).st_mtime,
            'Hash': data['Hash'],
        }

    for f in res['Delete']:
        print "Deleting file {}".format(f)

        os.remove(os.path.join(path, f))


def get_hash(f, path):
    """
    Get the sha1 hash and the body of the file passed in (relative to path)
    """
    m = hashlib.sha1()
    body = ""
    with open(os.path.join(path, f)) as f:
        body = f.read()
        m.update(body)
    return body, base64.b64encode(m.digest())


def main():
    """ MAIN FUNCTION """
    i = 0
    while True:
        i += 1
        print "Pulsing {}".format(i)
        pulse("test")
        time.sleep(0.5)

if __name__ == "__main__":
    main()
