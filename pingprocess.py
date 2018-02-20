#!/usr/bin/env python

import os, os.path, json
import shutil

METRIC_DIR = "metrics"
PROCESSED_DIR = "processed"
CONFIG_FILE = "synchronizer.json"
GROUP_BY = 200

def ls(dir):
    """
    List a directory, returning full path.
    """
    return ls_grep(dir, lambda x: True)

def ls_grep(dir, filter):
    """
    List a directory, returning full path.
    Only files that match the filter are returned.
    """
    return [os.path.join(dir, f) for f in os.listdir(dir) if filter(f)]

def get_metric_files():
    """
    Read logs.
    """
    d = {}
    dirs = ls(METRIC_DIR)

    for dir in dirs:
        # only list files that are not the config file
        files = ls_grep(dir, lambda x: x != CONFIG_FILE)
        d[dir] = files

    return d

def read_json(f):
    """
    Read a json file.
    """
    r = {}
    with open(f) as fd:
        r = json.load(fd)

    return r

def divide_logs(d):
    """
    Read a json file.
    """
	for dir in d:
		for file in d[dir]:
	        # read metric file
	    	j = read_json(file)

	    	# each entry

            for input in j:
            	




def main():
    """
    Main.
    """
    d = get_metric_files()
    d = divide_logs(d)
    save_logs(d)

main()
