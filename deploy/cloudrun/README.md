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
  --cpu-throttling --min-instances 0 --max-instances 1 \
  --concurrency 40 --timeout 3600
```

## 3. Spending cap

`budget-cap/install.sh <BILLING_ACCOUNT_ID>` installs a US$1 monthly budget
whose notifications reach a small Cloud Run function. When the month's spend
reaches the budget, the function removes public access from the service: the
site answers 403, compute stops, and nothing is deleted. It never disables
billing, which Google documents can delete resources. The installer ends with an
end-to-end test that simulates a US$2 month, checks for the 403, and restores
the site. Restore by hand with:

```sh
gcloud run services add-iam-policy-binding soilkeypro --region us-east1 \
  --member=allUsers --role=roles/run.invoker
```

## Cost / scaling knobs

The configuration above stays within the free tier for light academic use. The
earlier always-on setup (`--min-instances 1 --no-cpu-throttling`) cost about
US$8-15 a month and is not needed: request-based billing was tested and keeps
sessions working. To serve more concurrent users, raise `--max-instances` **and**
add `--session-affinity`. Bump `--memory` if the app runs out of memory
(`gcloud run services logs read soilkeypro --region us-east1`).
