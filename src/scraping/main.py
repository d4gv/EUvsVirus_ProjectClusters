#!/usr/bin/python
""" main

Crawler of Devost project sites from EUvsVirusHackathon for D4G
EUvsVirusCluster project.

Script crawls endpoints provided by devpost-api project by ViRb3
(https://github.com/ViRb3/devpost-api):
(1) docker pull virb3/devpost-api:latest
(2) docker run -p 5000:5000 virb3/devpost-api:latest
(3) Access API at http://127.0.0.1:5000
    - endpoints:
        /user/:username [NA for this project]
        /project/:project_name

Data are persisted as JSON dump in data directory.

Author: datadonk23
Date: 07.05.20 
"""

import json
import logging
logging.getLogger().setLevel(logging.INFO)

from request_util import make_request


def filter_response(resp):
    """ Filters requests according to desired schema.

    :param resp: requests.Response
    :return: Filtered data
    :return type: dict
    """
    data = {}
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


def perist_fetched_data(fetched_data, f_path):
    """ Persist fetched data as JSON file.

    :param f_path: Filepath of output file
    :type f_path: str
    :param fetched_data: Fetched data
    :type fetched_data: [Dict]
    :return: -
    """
    with open(f_path, "w") as f:
        json.dump(fetched_data, f)


if __name__ == "__main__":
    host = "http://127.0.0.1:5000"
    project_endpoint = "/project/"
    fetched_data = []
    output_fpath = "data/EUvsVirus_projects.json"

    #FIXME fetched proj names list goes here
    test_proj_list = ["zero_project",
                      "eunia-european-union-national-informal-assistance-oz4kcp",
                      "crowd-free-x08uy5", "covid-gur92q", "jobliebe",
                      "myminoritymatters"]

    for proj in test_proj_list:
        logging.info("Fetch data from " + str(proj))
        req_url = host + project_endpoint + proj
        resp = make_request(req_url)
        fetched_data.append(filter_response(resp))

    perist_fetched_data(fetched_data,output_fpath)
