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

Authors: jill-augustine, datadonk23
Date: 07.05.20
"""

import os
import json
import logging
import pandas as pd
from request_util import make_request, filter_response

logging.getLogger().setLevel(logging.INFO)

HOST = "http://127.0.0.1:5000"
PROJECT_ENDPOINT = "/project/"


def get_filtered_data(request_url, project):
    """
    Given a project name, returns filtered response and success indicator.

    :param request_url: the url to make the request to
    :type request_url:  str
    :param project:     project name
    :type project:      str
    :return:            tuple of response and success indicator
    """
    logging.info("Fetch data from %s", str(project))

    response = make_request(request_url + project)

    filtered_response = filter_response(response)
    filtered_response["proj_url"] = "https://devpost.com/software/" + project

    return (filtered_response, response is not None)


def persist_fetched_data(fetched_data, file_path):
    """
    Persist fetched data as JSON file.

    :param file_path:    Filepath of output file
    :type file_path:     str
    :param fetched_data: Fetched data
    :type fetched_data:  [Dict]
    :return:             -
    """
    with open(file_path, "w") as output_file:
        json.dump(fetched_data, output_file)


if __name__ == "__main__":
    """ Main program loop """

    # prepare project endpoints
    request_url = HOST + PROJECT_ENDPOINT

    data_dir = os.path.join(os.environ.get("HOME"), "Dropbox", "EUvsVirus")
    output_file_path = os.path.join(data_dir, "EUvsVirus_projects.json")

    project_urls_df = pd.read_csv(os.path.join(data_dir, "projectURLS.csv"))
    project_names = (
        project_urls_df
        .ProjURL
        .str.extract(
            r"(?<=https://devpost.com/software/)(.+$)", expand=False)
        .tolist())
    projects = len(project_names)

    # query project endpoints and persist data
    all_responses = []
    for i in range(projects):
        logging.info("Project %s of %s", str(i + 1), str(projects))
        all_responses.append(get_filtered_data(request_url, project_names[i]))

    persist_fetched_data([x[0] for x in all_responses], output_file_path)

    all_responses_df = (
        pd.DataFrame
        .from_records(all_responses, columns=["Response", "Success"]))
    logging.info(
        "%s of %s URLs were successfully scraped.",
        str(all_responses_df.Success.sum()),
        str(len(all_responses_df)))

    # write results
    all_data = (
        project_urls_df
        .merge(
            all_responses_df.Response.apply(pd.Series),
            left_on="ProjURL",
            right_on="proj_url",
            how="left")
        .drop(columns="proj_url"))
    all_data.to_csv(
        os.path.join(data_dir, "all_data.tsv"),
        sep="\t",
        index=False)
