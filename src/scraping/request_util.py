#!/usr/bin/python
""" request_util

Utility methods for making requests.

Author: datadonk23
Date: 07.05.20 
"""

import requests as req

def make_request(url):
    """ Request to API endpoint

    :param url: URL
    :type url: str
    :return: response
    """
    resp = None
    try:
        resp = req.get(url, timeout=3)
        resp.raise_for_status()
    except req.exceptions.HTTPError as ehttp:
        print("Http Error:", ehttp)
    except req.exceptions.ConnectionError as econ:
        print("Error Connecting:", econ)
    except req.exceptions.Timeout as etime:
        print("Timeout Error:", etime)
    except req.exceptions.RequestException as ereq:
        print("OOps: Something Else", ereq)

    return resp

def filter_response(resp):
    """ Filters requests according to desired schema.

    :param resp: requests.Response
    :return: Filtered data
    :return type: dict
    """
    data = {}
    
    if resp is not None:
        json_resp = resp.json()

        if json_resp["title"]:
            data["title"] = json_resp["title"]
        else:
            data["title"] = None

        if json_resp["text"]:
            data["text"] = json_resp["text"]
        else:
            data["text"] = None
    
    return data

    