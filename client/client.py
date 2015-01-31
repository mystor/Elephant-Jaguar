"""--"""

import requests
import os
import json
from datetime import datetime

SERVER = "localhost:8000"

def pulse(path):
    request = {}

    files = os.listdir(path)
    for f in files:
        s = os.stat(f)
        mod_time = datetime(s.st_mtime)
        request[f] = {
            'modified': mod_time
        }

    res = requests.post(SERVER+"/syncSaved/", data=json.dumps(request))
    json.loads(res.text)
