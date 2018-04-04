#!/usr/bin/env python

import os, os.path, json
import shutil

METRIC_DIR = "metrics"
PROCESSED_DIR = "processed"
CONFIG_FILE = "synchronizer.json"
TS="ts"
VAL="val"
TIME_UNIT = "ms"

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
    Return a dictionary from run (unique timestamp)
    to list of metric files.
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

def key(config):
    """
    Create a key from a config file.
    """

    # get start_time from config
    start_time = config["start_time"]

    keys = [
        "trcb_exp_mode",
        "trcb_exp_node_number",
        "trcb_exp_default_event_interval",
        "trcb_exp_drop_ratio",
        "trcb_exp_latency",
        "trcb_exp_node_event_number"
    ]

    l = []
    for k in keys:
        l.append(str(config[k]))

    k = "~".join(l)
    return (start_time, k)

def group_by_config(d):
    """
    Given metric files, group them by config file.
    """
    r = {}
    
    for dir in d:
        config_path = os.path.join(dir, CONFIG_FILE)
        (start_time, k) = key(
            read_json(config_path)
        )
        
        # create empty dictionary if key not already in dictionary
        if not k in r:
            r[k] = {}

        for file in d[dir]:
            # read metric file
            j = read_json(file)

            # for all time-series types
            # for all metrics remove start_time
            for type, typev in j.iteritems():

                # create empty list if type not already in dictionary
                if not type in r[k]:
                    r[k][type] = {}

                for subtype, subtypev in typev.iteritems():
                    for entry in subtypev:
                        entry[TS] -= start_time

                    # create empty list if type not already in dictionary
                    if not subtype in r[k][type]:
                        r[k][type][subtype] = []

                    # store metrics by type
                    r[k][type][subtype].append(j[type][subtype])

    return r

def get_highest_ts(entries):
    """
    Find the higher timestamp of all entries.
    """
    highest = 0
    for list in entries:
        for entry in list:
            highest = max(highest, entry[TS])
    return highest

def bottom_val(type):
    """
    Return bottom val depending on the type passed as input.
    """
    one = ["transmission"]
    two = ["memory"]
    three = ["processing"]

    if type in one:
        return 0
    if type in two:
        return 0
    if type in three:
        return 0

    print("type not found. Exiting.")
    exit()

def add(type, valA, valB):
    """
    Sum two vals
    """

    one = ["transmission"]
    two = ["memory"]
    three = ["processing"]

    if type in one:
        return valA + valB
    if type in two:
        return valA + valB
    if type in three:
        return valA + valB

    print("type not found. Exiting.")
    exit()

def default(type, previous):
    """
    Default value given a type:
    - if transmission, 0
    - if memory, previous value
    - if processing, 0
    """
    one = ["transmission"]
    two = ["memory"]
    three = ["processing"]

    if type in one:
        return 0
    if type in two:
        return previous
    if type in three:
        return 0

    print("type not found. Exiting.")
    exit()

def ignore_pre_big_bang(run):
    """
    Remove metrics before timestamp 0.
    """

    return [entry for entry in run if entry[TS] >= 0]

def assume_unknown_values(d):
    """
    Assume values for timestamps not reported for transmission graphs.
    """

    # for each key: configuration
    for key in d:

        # get all time-series types
        types = d[key].keys()

        # type is one of: processing transmission memory
        for type in types:

            subtypes = d[key][type].keys()
            
            for subtype in subtypes:

                # find the highest timestamp of all runs for this type
                highest_ts = get_highest_ts(d[key][type][subtype])

                runs = []

                for run in d[key][type][subtype]:

                    # remove timestamps before 0
                    run = ignore_pre_big_bang(run)

                    # get bottom val
                    bs = bottom_val(type)

                    # since we can have several metrics
                    # for the same timestamp,
                    # aggregate metrics per timestamp
                    ts_to_val = {}

                    for metric in run:
                        ts = metric[TS]
                        val = metric[VAL]

                        # if ts not in map
                        # create an entry
                        if not ts in ts_to_val:
                            ts_to_val[ts] = bs
                        else:
                            if type in ["transmission"]:
                                ts_to_val[ts] = add(type, ts_to_val[ts], val)
                            else:
                                ts_to_val[ts] = add(type, ts_to_val[ts], val)

                    previous = bs
                    
                    # create bottom values for unknown timestamps
                    for ts in range(0, highest_ts):
                        if not ts in ts_to_val:
                            ts_to_val[ts] = default(type, previous)

                        previous = ts_to_val[ts]

                    # store the ts_to_val map
                    runs.append(ts_to_val)

                # update all runs
                d[key][type][subtype] = runs

    return d

def sum_lists(ls):
    """
    Sum two lists.
    """
    return [sum(x) for x in zip(*ls)]

def divide_list_by(ls, n):
    """
    Divide all elements of list by n.
    """
    return [e / n for e in ls]

def average(d):
    """
    Average runs.
    """

    for key in d:
        print("key is %s" % key)

        # get all time-series types
        types = d[key].keys()
        
        for type in types:

            print("type is %s" % type)

            subtypes = d[key][type].keys()
            
            for subtype in subtypes:

                print("subtype is %s" % subtype)

                # number of runs
                runs_number = len(d[key][type][subtype])

                # number of metrics
                metrics_number = len(d[key][type][subtype][0]) - 1

                print("metrics_number is %s" % metrics_number)

                # get bottom size
                bs = bottom_val(type)

                print("runs number is %s" % runs_number)

                # list where we'll store the sum of the vals
                sum = [bs for i in range(0, metrics_number)]

                # sum all runs
                for run in d[key][type][subtype]:

                    for i in range(0, metrics_number):
                        sum[i] += run[i]

                # avg of sum
                # avg = [s / runs_number for s in sum]
                avg = [entry/runs_number for entry in sum]

                print("avg is %s" % avg)

                # store avg
                d[key][type][subtype] = avg

    return d

def to_ms(microseconds):
    """
    Converts microseconds to milliseconds.
    """
    return microseconds / float(1000)

def aggregate(d):
    """
    Aggregate types of the same run.
    """

    r = {}

    for key in d:
        # create key in dictionary
        r[key] = {}

        # init
        for type in d[key]:
            for subtype in d[key][type]:
                r[key][subtype] = [l for l in d[key][type][subtype]]

        # sum all lists that have these types
        # to_sum = []
        # for type in d[key]:
        #     if type in ["tcbcast", "tcbcast_ack"]:
        #         # make list of lists into list
        #         ls = [e["val"] for l in d[key][type] for e in l]
        #         to_sum.append(ls)

        #r[key]["transmission"] = sum_lists(to_sum)
        # r[key]["tcbcast"] = [e["val"] for l in d[key]["tcbcast"] for e in l]
        # r[key]["tcbcast_ack"] = [e["val"] for l in d[key]["tcbcast_ack"] for e in l]
        # r[key]["memory_crdt"] = []
        # r[key]["memory_algorithm"] = []
        # r[key]["latency_send"] = []
        # r[key]["latency_deliver"] = []

        # # aggregate memory values
        # for [C, R] in d[key]["memory"]:
        #     r[key]["memory_crdt"].append(C)
        #     r[key]["memory_algorithm"].append(R)
        
        # # aggregate latency values
        # for lord in d[key]["processing"]: # local or remote dict
        #     for lort in lord: # local or remote type
        #         k = "processing_" + lort
        #         latency_values = lord[lort]
        #         # latency_values = map(to_ms, lord[lort])
        #         r[key][k].extend(latency_values)

    return r

def group_by_simulation(d):
    """
    Group metrics by simulation (gset, awset, ...).
    """

    r = {}

    for type in d:
        simulation = type.split("~")[0]

        if not simulation in r:
            r[simulation] = {}

        r[simulation][type] = d[type]

    return r

def save_file(path, content):
    """
    Save content in path.
    """

    dir = os.path.dirname(path)

    # ensure directory exist
    if not os.path.exists(dir):
        os.makedirs(dir)

    # write content
    with open(path, "w") as fd:
        fd.write(content)

def dump(d):
    """
    Save average to files.
    """

    # clear folder
    shutil.rmtree(PROCESSED_DIR, ignore_errors=True)

    for simulation in d:
        for type in d[simulation]:
            path = os.path.join(*[PROCESSED_DIR, simulation, type])
            content = json.dumps(d[simulation][type])
            save_file(path, content)

def main():
    """
    Main.
    """
    d = get_metric_files()
    d = group_by_config(d)
    d = assume_unknown_values(d)
    d = average(d)
    # print(d)
    d = aggregate(d)
    d = group_by_simulation(d)
    dump(d)

main()
