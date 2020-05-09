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
    try:
        resp = req.get(url, timeout=3)
        resp.raise_for_status()
    except req.exceptions.HTTPError as ehttp:
        print("Http Error:", ehttp)
    except req.exceptions.ConnectionError as econ:
        print("Error Connecting:", econ)
    except requests.exceptions.Timeout as etime:
        print("Timeout Error:", etime)
    except requests.exceptions.RequestException as ereq:
        print("OOps: Something Else", ereq)

    return resp
