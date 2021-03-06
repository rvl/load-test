# a bit of copy&paste and adaption from:
# https://github.com/locustio/locust/pull/622

from locust import HttpLocust, TaskSet, task, web, runners
from flask import jsonify, make_response
from itertools import chain
import six

@web.app.route("/metrics")
def prometheus_metrics():
    is_distributed = isinstance(runners.locust_runner, runners.MasterLocustRunner)
    if is_distributed:
        slave_count = runners.locust_runner.slave_count
    else:
        slave_count = 0

    if runners.locust_runner.host:
        host = runners.locust_runner.host
    elif len(runners.locust_runner.locust_classes) > 0:
        host = runners.locust_runner.locust_classes[0].host
    else:
        host = None

    state = 1
    if runners.locust_runner.state != "running":
        state = 0

    rows = []
    for s in chain(_sort_stats(runners.locust_runner.request_stats), [runners.locust_runner.stats.aggregated_stats("Total", full_request_history=True)]):
        if s.name != "Total":
            rows.append("locust_request_count{{endpoint=\"{}\", method=\"{}\"}} {}\n"
                        "locust_request_per_second{{endpoint=\"{}\"}} {}\n"
                        "locust_failed_requests{{endpoint=\"{}\", method=\"{}\"}} {}\n"
                        "locust_average_response{{endpoint=\"{}\", method=\"{}\"}} {}\n"
                        "locust_average_content_length{{endpoint=\"{}\", method=\"{}\"}} {}\n"
                        "locust_max_response_time{{endpoint=\"{}\", method=\"{}\"}} {}\n"
                        "locust_running{{site=\"{}\"}} {}\n"
                        "locust_workers{{site=\"{}\"}} {}\n"
                        "locust_users{{site=\"{}\"}} {}\n".format(
                            s.name,
                            s.method,
                            s.num_requests,
                            s.name,
                            s.total_rps,
                            s.name,
                            s.method,
                            s.num_failures,
                            s.name,
                            s.method,
                            s.avg_response_time,
                            s.name,
                            s.method,
                            s.avg_content_length,
                            s.name,
                            s.method,
                            s.max_response_time,
                            host,
                            state,
                            host,
                            slave_count,
                            host,
                            runners.locust_runner.user_count,
                        )
            )

    response = make_response("".join(rows))
    response.mimetype = "text/plain; charset=utf-8'"
    response.content_type = "text/plain; charset=utf-8'"
    response.headers["Content-Type"] = "text/plain; charset=utf-8'"
    return response

def _sort_stats(stats):
    return [stats[key] for key in sorted(six.iterkeys(stats))]
