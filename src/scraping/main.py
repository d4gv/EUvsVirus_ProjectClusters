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

import os
import json
import logging
logging.getLogger().setLevel(logging.INFO)

from request_util import make_request, filter_response
import pandas as pd

def get_filtered_data(proj):
    '''Given a project name, returns filtered response and success indicator
    
    :param proj: project name
    :type proj: str
    :return: tuple of response and success indicator'''
    logging.info("Fetch data from " + str(proj))

    req_url = host + project_endpoint + proj
    resp = make_request(req_url)

    filtered_response = filter_response(resp)
    filtered_response['proj_url'] = "https://devpost.com/software/"+proj

    success =  True if resp is not None else False
    
    return (filtered_response, success)

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
    data_dir = os.path.join(os.environ["HOME"],"Dropbox","EUvsVirus")
    output_fpath = os.path.join(data_dir,"EUvsVirus_projects.json")

    projectURLS_df = pd.read_csv(os.path.join(data_dir,"projectURLS.csv"))
    proj_names = projectURLS_df.ProjURL.str.extract(r"(?<=https://devpost.com/software/)(.+$)",expand=False).tolist()

    all_responses = [None for x in range(len(proj_names))]
    for i in range(len(proj_names)):
        print(f"Project {i+1} of {len(proj_names)}")
        all_responses[i] = get_filtered_data(proj_names[i])

    perist_fetched_data([x[0] for x in all_responses],output_fpath)

    all_responses_df = pd.DataFrame.from_records(all_responses, columns=['Response','Success'])
    print(f'{all_responses_df.Success.sum()} of {len(all_responses_df)} URLs were successfully scraped.')

    all_data = (projectURLS_df.merge(all_responses_df.Response.apply(pd.Series), 
                                    left_on='ProjURL', right_on='proj_url', how='left')
                              .drop(columns='proj_url')
    )
    all_data.to_csv(os.path.join(data_dir,'all_data.tsv'), sep='\t', index=False)
