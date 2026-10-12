# Deploying soilKey Pro to Google Cloud Run (soilkeypro.com)

The public Shiny app runs as a container (shared, host-agnostic
[`deploy/fly/Dockerfile`](../fly/Dockerfile)) on Cloud Run, in project
`soilkeypro`, region `us-east1` (a Cloud Run domain-mapping region; São Paulo
is not, which is why we're here rather than closer to home).

## Project / prerequisites (one-time, already done)

```sh
gcloud projects create soilkeypro --name="soilKey Pro"
gcloud billing projects link soilkeypro --billing-account=017E7E-3F15CB-C4A07F   # "flora"
gcloud config set project soilkeypro
gcloud services enable run.googleapis.com cloudbuild.googleapis.com artifactregistry.googleapis.com
gcloud artifacts repositories create soilkey --repository-format=docker --location=us-east1
```

## 1. Build the image (Cloud Build -> Artifact Registry)

```sh
gcloud builds submit --config deploy/cloudrun/cloudbuild.yaml \
  --substitutions _IMAGE=us-east1-docker.pkg.dev/soilkeypro/soilkey/app:latest \
  --project soilkeypro .
```

The build runs on Cloud Build's default machine (e2-standard-2), the only type
in its free tier of 2,500 build-minutes a month per billing account. Builds
count against the US$2 monthly cap below, and the larger E2_HIGHCPU_8 used
until October 2026 cost US$0.016 a minute (about US$0.36 for one slow build).

## 2. Deploy to Cloud Run

Request-based billing, scaling to zero. CPU is allocated only while a request is
being served, which keeps an academic-traffic app inside the Cloud Run free
tier. Verified on 0.9.204/0.9.205: a Shiny session left idle for over a minute
stays connected and reactive, and a cold start (instance start to `Listening`)
takes about 5 seconds.

```sh
gcloud run deploy soilkeypro \
  --image us-east1-docker.pkg.dev/soilkeypro/soilkey/app:latest \
  --region us-east1 --project soilkeypro \
  --allow-unauthenticated \
  --port 8080 --cpu 1 --memory 2Gi \
  --cpu-throttling --min-instances 0 --max-instances 3 --session-affinity \
  --concurrency 40 --timeout 3600
```

Up to three instances, with session affinity. Each instance is one R process,
and an open Shiny session holds a request (its websocket) for as long as the tab
is open. With a single instance, 40 open tabs, or one heavy computation, left
new visitors with HTTP 429 "no available instance". Extra instances start only
under load and stop when idle, so a quiet month costs the same. Session
affinity keeps each browser on the instance that holds its session, which
Shiny needs for downloads and widget data. Sessions that share an instance
still wait for each other's computations: that is R, not Cloud Run.

## 3. Spending cap

`budget-cap/install.sh <BILLING_ACCOUNT_ID>` installs a US$2 monthly budget
(US$1 until October 2026) whose notifications reach a small Cloud Run function.
When the month's spend reaches the budget, the function removes public access
from the service: the site answers 403, compute stops, and nothing is deleted.
It never disables billing, which Google documents can delete resources. The
function reads the budget from each notification, so changing the amount of the
budget is all it takes to move the cap:

```sh
gcloud billing budgets list --billing-account <BILLING_ACCOUNT_ID>
gcloud billing budgets update <BUDGET_ID> --billing-account <BILLING_ACCOUNT_ID> \
  --budget-amount 2USD
```

The installer ends with an end-to-end test that simulates a US$3 month, checks
for the 403, and restores the site. Restore by hand with:

```sh
gcloud run services add-iam-policy-binding soilkeypro --region us-east1 \
  --member=allUsers --role=roles/run.invoker
```

## 4. Uptime check

`.github/workflows/uptime.yaml` asks for the home page once an hour (minute 17)
and fails if the app does not answer within three tries; GitHub e-mails a failed
scheduled run to whoever last changed its schedule. It catches billing switched
off (500), the spending cap above (403), a revision that does not start, and DNS
or certificate trouble. It runs free on the public repository, and a check that
meets a scaled-to-zero instance costs one cold start, well inside the free tier.
Google Cloud Monitoring could do the same, but its alerting policies are billed
per condition and would count against the US$2 cap. GitHub pauses scheduled
workflows after 60 days without repository activity and says so by e-mail; any
push, or "Enable workflow" in the Actions tab, resumes it.

## 5. Image clean-up

Every build pushes an image of about 1.2 GB to Artifact Registry, where storage
is billed by the gigabyte. Until October 2026 the repository kept every tagged
image, and each release is tagged with its version: by 2026-10-12 it held 21
images, about 24 GB, which cost about US$0.05 a day and had taken the month to
US$0.94 of the US$1 cap of the time. `cleanup-policy.json` keeps what the
service can need and lets the registry delete the rest:

| Rule | What it does |
|---|---|
| `keep-live` | keeps the image tagged `live` |
| `keep-2-most-recent` | keeps the two newest images, whatever their tags: the release and the one before it, for a rollback |
| `delete-the-rest` | deletes any other image more than a day old |

A keep rule wins over a delete rule. Apply it once; it replaces the policies the
repository has:

```sh
gcloud artifacts repositories set-cleanup-policies soilkey \
  --location us-east1 --project soilkeypro \
  --policy deploy/cloudrun/cleanup-policy.json --no-dry-run
gcloud artifacts repositories list-cleanup-policies soilkey \
  --location us-east1 --project soilkeypro
```

The `live` tag is what protects the image in production. After a release takes
the traffic, move the tag to it:

```sh
gcloud artifacts docker tags add \
  us-east1-docker.pkg.dev/soilkeypro/soilkey/app:<version> \
  us-east1-docker.pkg.dev/soilkeypro/soilkey/app:live
```

Without the tag, the image in production is safe only while it is one of the two
newest. Two builds pushed and not promoted would let the registry delete it, and
the next instance of the service would fail to start. A tag that was not moved
after a release stays on the previous image, which is then kept as well: more
storage, no outage.

## Cost / scaling knobs

The configuration above stays within the free tier for light academic use. The
earlier always-on setup (`--min-instances 1 --no-cpu-throttling`) cost about
US$8-15 a month and is not needed: request-based billing was tested and keeps
sessions working. To serve more concurrent users, raise `--max-instances`
(keep `--session-affinity`). Every instance counts against the US$2 cap, so a
burst of visitors can reach it sooner. Bump `--memory` if the app runs out of
memory (`gcloud run services logs read soilkeypro --region us-east1`).
