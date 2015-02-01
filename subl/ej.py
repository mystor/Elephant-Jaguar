### BEGIN NOT MAH CODE ###
import sys
import os.path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import re
import requests
from requests.status_codes import codes
try:
    import http.client as httplib
except ImportError:
    import httplib
import commandline
import sublime, sublime_plugin
from io import BytesIO
import logging

logging.basicConfig(format='%(asctime)s %(message)s')
logger = logging.getLogger()


class CurlSession(object):
    ERR_UNKNOWN_CODE = "Curl failed with an unrecognized code"
    CURL_ERRORS = {
        2: "Curl failed initialization.",
        5: "Curl could not resolve the proxy specified.",
        6: "Curl could not resolve the remote host.\n\nPlease verify that your Internet"
           " connection works properly."
    }

    class FakeSocket(BytesIO):
        def makefile(self, *args, **kw):
            return self

    def __init__(self, verify=None):
        self.verify = verify

    def _parse_http(self, text):
        # if the response text starts with a 302, skip to the next non-302 header
        text = str(text, encoding='utf-8')
        if re.match(r'^HTTP/.*?\s302 Found', text):
            m = re.search(r'(HTTP/\d+\.\d+\s(?!302 Found).*$)', text, re.S)
            if not m:
                raise Exception("Unrecognized response: %s" % text)
            else:
                text = m.group(1)

        # if the response text starts with a "200 Connection established" but continues with a 201,
        # skip the 200 header. This happens when using a proxy.
        #
        # e.g. HTTP/1.1 200 Connection established
        #       Via: 1.1 proxy
        #       Connection: Keep-Alive
        #       Proxy-Connection: Keep-Alive
        #
        #       HTTP/1.1 201 Created
        #       Server: GitHub.com
        #       ...
        #       Status: 201 Created
        #       ...
        if re.match(r'^HTTP/.*?\s200 Connection established', text):
            m = re.search(r'(HTTP/\d+\.\d+\s(?!200 Connection established).*$)', text, re.S)
            if not m:
                raise Exception("Unrecognized response: %s" % text)
            else:
                text = m.group(1)

        # remove Transfer-Encoding: chunked header, as it causes reading the response to fail
        # first do a quick check for it, so we can avoid doing the expensive negative-lookbehind
        # regex if we don't need it
        if "Transfer-Encoding: chunked" in text:
            # we do the negative-lookbehind to make sure we only strip the Transfer-Encoding
            # string in the header
            text = re.sub(r'(?<!\r\n\r\n).*?Transfer-Encoding: chunked\r\n', '', text, count=1)

        logger.debug("CurlSession - getting socket from %s" % text)
        socket = self.FakeSocket(text.encode())
        response = httplib.HTTPResponse(socket)
        response.begin()
        return response

    def _build_response(self, text):
        logger.debug("CurlSession: building response from %s" % text)
        raw_response = self._parse_http(text)
        response = requests.models.Response()
        response.encoding = 'utf-8'
        response.status_code = raw_response.status
        response.headers = dict(raw_response.getheaders())
        response._content = raw_response.read()
        return response

    def request(self, method, url, headers=None, params=None, data=None, auth=None, allow_redirects=False, config=None, proxies=None):
        try:
            curl = commandline.find_binary('curl')
        except commandline.BinaryNotFoundError:
            sublime.error_message("I couldn't find \"curl\" on your system. Curl is required on Linux. Please install it and try again.")
            return

        curl_options = ['-i', '-L', '--user-agent', 'Sublime Github', '-s']
        if auth:
            curl_options.extend(['--user', "%s:%s" % auth])
        if self.verify:
            curl_options.extend(['--cacert', self.verify])
        if headers:
            for k, v in headers.items():
                curl_options.extend(['-H', "%s: %s" % (k, v)])
        if method in ('post', 'patch'):
            curl_options.extend(['-d', data])
        if method == 'patch':
            curl_options.extend(['-X', 'PATCH'])
        if params:
            url += '?' + '&'.join(['='.join([k, str(v)]) for k, v in params.items()])
        if proxies and proxies.get('https', None):
            curl_options.extend(['-x', proxies['https']])

        command = [curl] + curl_options + [url]

        logger.debug("CurlSession: invoking curl with %s" % command)
        try:
            command_response = commandline.execute(command)
        except commandline.CommandExecutionError as e:
            logger.error("Curl execution: %s" % repr(e))
            self._handle_curl_error(e.errorcode)
            return

        response = self._build_response(command_response)
        response.url = url
        return response

    def post(self, *args, **kwargs):
        return self.request("post", *args, **kwargs)

    def _handle_curl_error(self, error):
        sublime.error_message(
            self.CURL_ERRORS.get(error, "%s: %s" % (self.ERR_UNKNOWN_CODE, error)))


def session(verify=None, force_curl=False):
    if not force_curl and hasattr(httplib, "HTTPSConnection"):
        session = requests.Session()
        session.verify = verify
        return session
    else:  # try curl
        return CurlSession(verify=verify)


### END NOT MAH CODE ###

import hashlib
import os
import json
import base64
#import sys
import threading
import time

SERVER = "http://localhost:8000"

# class EJLoop(threading.Thread):
#     def __init__(self, view):
#         self.view = view
#         threading.Thread.__init__(self)

#     def run(self):
#         while True:
#             m = hashlib.sha1()
#             body = self.view.substr(sublime.Region(0, self.view.size()))
#             m.update(body)
#             hash = base64.b64encode(m.digest())

#             if self.view.ej_locked:
#                 request = {
#                     'Key': file,
#                     'Updated': {
#                         'Hash': hash,
#                         'Data': body
#                     }
#                 }
#                 print "Push Response:", requests.post(SERVER+"/push", data=json.dumps(request)).text
#             else:
#                 request = {
#                     'Key': file,
#                     'Target': { 'Hash': hash }
#                 }

#                 res = json.loads(requests.post(SERVER+"/watch", data=json.dumps(request)).text)
#                 self.view.set_read_only(res['Locked'])

#                 if res['Locked']:
#                     edit = self.view.begin_edit()
#                     self.view.replace(edit,
#                                       sublime.Region(0, self.view.size()),
#                                       res['Target']['Data'])
#                     self.view.end_edit(edit)
#             time.sleep(0.2)

LOOP_DELAY = 250

def mk_ej_loop(view):
    def ej_loop():
        if view.settings().get('ej_done'):
            return
        m = hashlib.sha1()
        body = view.substr(sublime.Region(0, view.size()))
        m.update(body)
        hsh = base64.b64encode(m.digest())
        f = os.path.basename(view.file_name())

        if view.settings().get('ej_locked'):
            print "locked"
            request = {
                'Key': f,
                'Updated': {
                    'Hash': hsh,
                    'Data': body
                }
            }
            class T(threading.Thread):
                def run(self):
                    print "Push Response:", requests.post(SERVER+"/push", data=json.dumps(request)).text
                    sublime.set_timeout(ej_loop, LOOP_DELAY)
            T().start()
        else:
            print "unlocked"
            request = {
                'Key': f,
                'Target': { 'Hash': hsh }
            }

            class T(threading.Thread):
                def run(self):
                    res = json.loads(requests.post(SERVER+"/watch", data=json.dumps(request)).text)
                    def update():
                        print "Res is locked?: " + str(res['Locked'])
                        view.set_read_only(False)
                        if res['Locked']:
                            edit = view.begin_edit()
                            print "targ_data", res['Target']
                            view.replace(edit, sublime.Region(0, view.size()),
                                         res['Target']['Data'])
                            view.end_edit(edit)
                        view.settings().set('ej_locked', False)
                        view.set_read_only(res['Locked'])
                        sublime.set_timeout(ej_loop, LOOP_DELAY)
                    sublime.set_timeout(update, 0)
            T().start()

    return ej_loop

class ElephantJaguar(sublime_plugin.EventListener):
    def on_post_save(self, view):
        if not (view.file_name()
                and os.path.exists(os.path.join(os.path.dirname(view.file_name()), ".ej"))):
            return

        view.settings().set('ej_locked', False)

        f = os.path.basename(view.file_name())
        request = {
            'Key': f
        }

        # Make a request to the unlock server, sending the key to unlock
        print "Unlock Response:", requests.post(SERVER+"/unlock", data=json.dumps(request)).text

    def on_modified(self, view):
        if not (view.file_name()
                and os.path.exists(os.path.join(os.path.dirname(view.file_name()), ".ej"))):
            return
        if view.is_read_only():
            return
        print "Here"
        view.settings().set('ej_locked', True)

    def on_close(self, view):
        view.settings().set('ej_done', True)

    def on_load(self, view):
        view.settings().set('ej_done', False)
        view.settings().set('ej_locked', False)

        if not (view.file_name()
                and os.path.exists(os.path.join(os.path.dirname(view.file_name()), ".ej"))):
            return

        mk_ej_loop(view)()
