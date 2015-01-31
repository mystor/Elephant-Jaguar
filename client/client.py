"""--"""

import requests
import os
import json
from datetime import datetime


####
from dateutil.tz import tzutc

UTC = tzutc()

def serialize_date(dt):
    """
    Serialize a date/time value into an ISO8601 text representation
    adjusted (if needed) to UTC timezone.

    For instance:
    >>> serialize_date(datetime(2012, 4, 10, 22, 38, 20, 604391))
    '2012-04-10T22:38:20.604391Z'
    """
    if dt.tzinfo:
        dt = dt.astimezone(UTC).replace(tzinfo=None)

    return dt.isoformat() + 'Z'
####

SERVER = "http://localhost:8000"

def pulse(path):
    request = {}

    # os.stat_float_times(False)

    files = os.listdir(path)
    for f in files:
        s = os.stat(os.path.join(path, f))
        mod_time = datetime.fromtimestamp(s.st_mtime)
        request[f] = {
            'modified': mod_time
        }

    res = requests.post(SERVER+"/sync", data=json.dumps(request, default=serialize_date))
    print json.loads(res.text)

pulse(os.environ['HOME'] + "/test")
