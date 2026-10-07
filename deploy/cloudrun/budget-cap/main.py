"""Take soilkeypro.com offline when the month's spend reaches the budget.

A Cloud Billing budget publishes its running total to a Pub/Sub topic several
times a day. This function reads that total and, once it reaches the budget,
removes public access from the Cloud Run service, so the site answers 403 and
stops consuming compute, which is almost all of its cost.

It deliberately does NOT disable billing. Google documents that disabling
billing "might irretrievably delete" resources; here the service, its image and
its secrets all stay intact, and one command brings the site back:

    gcloud run services add-iam-policy-binding soilkeypro --region us-east1 \
        --member=allUsers --role=roles/run.invoker

The service account running this function holds roles/run.admin on this one
service only, not on the project.
"""
import base64
import json
import os

import functions_framework
from google.cloud import run_v2
from google.iam.v1 import iam_policy_pb2

SERVICE = os.environ.get(
    "CAP_SERVICE", "projects/soilkeypro/locations/us-east1/services/soilkeypro")


def over_budget(message_data: str) -> tuple[bool, float, float]:
    """Parse a budget notification; True when the spend has reached the budget."""
    data = json.loads(base64.b64decode(message_data).decode("utf-8"))
    cost, budget = float(data["costAmount"]), float(data["budgetAmount"])
    return cost >= budget, cost, budget


def withdraw_public_access(policy) -> bool:
    """Remove allUsers from roles/run.invoker in place; True if anything changed."""
    changed = False
    for binding in policy.bindings:
        if binding.role == "roles/run.invoker" and "allUsers" in binding.members:
            binding.members.remove("allUsers")
            changed = True
    return changed


@functions_framework.cloud_event
def cap(cloud_event):
    hit, cost, budget = over_budget(cloud_event.data["message"]["data"])
    if not hit:
        print(f"spend {cost:.2f} below budget {budget:.2f}: nothing to do")
        return
    client = run_v2.ServicesClient()
    policy = client.get_iam_policy(
        request=iam_policy_pb2.GetIamPolicyRequest(resource=SERVICE))
    if withdraw_public_access(policy):
        client.set_iam_policy(
            request=iam_policy_pb2.SetIamPolicyRequest(resource=SERVICE, policy=policy))
        print(f"spend {cost:.2f} reached budget {budget:.2f}: public access removed")
    else:
        print(f"spend {cost:.2f} reached budget {budget:.2f}: site already private")
