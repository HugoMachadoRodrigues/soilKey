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

Single always-on instance so Shiny sessions never migrate (no session
affinity needed) and there is no cold start; CPU always allocated so the
websocket/reactives stay live between requests.

```sh
gcloud run deploy soilkeypro \
  --image us-east1-docker.pkg.dev/soilkeypro/soilkey/app:latest \
  --region us-east1 --project soilkeypro \
  --allow-unauthenticated \
  --port 8080 --cpu 1 --memory 2Gi \
  --min-instances 1 --max-instances 1 --no-cpu-throttling \
  --concurrency 40 --timeout 3600
```

`gcloud run services describe soilkeypro --region us-east1 --format='value(status.url)'`
gives the live `https://soilkeypro-*.run.app` URL to smoke-test first.

## 3. Custom domain (soilkeypro.com) + managed TLS

DNS is at Cloudflare. Cloud Run needs the domain verified once (Search Console
TXT), then maps it and issues a Google-managed certificate.

```sh
gcloud beta run domain-mappings create --service soilkeypro \
  --domain soilkeypro.com --region us-east1 --project soilkeypro
gcloud beta run domain-mappings create --service soilkeypro \
  --domain www.soilkeypro.com --region us-east1 --project soilkeypro
```

Add the A/AAAA records it prints in the Cloudflare DNS for `soilkeypro.com`,
**DNS only (grey cloud)** so Google can issue the cert. Check status:

```sh
gcloud beta run domain-mappings describe --domain soilkeypro.com \
  --region us-east1 --project soilkeypro
```

## Cost / scaling knobs

Always-on 1 vCPU / 2 GiB with CPU always allocated ≈ US$8-15/mo. To cut cost,
drop `--no-cpu-throttling` and set `--min-instances 0` (adds a cold start of
~20-40 s on the first hit). To serve more concurrent users, raise
`--max-instances` **and** add `--session-affinity`. Bump `--memory` if the app
OOMs (`gcloud run services logs read soilkeypro --region us-east1`).
