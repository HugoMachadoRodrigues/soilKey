#!/bin/sh
# Install the US$1 spending cap for soilkeypro.com. Run once billing is enabled:
#
#   sh deploy/cloudrun/budget-cap/install.sh <BILLING_ACCOUNT_ID>
#
# When the month's spend reaches US$1 the cap removes public access from the
# Cloud Run service (the site answers 403). It never disables billing and never
# deletes anything. Restore with:
#   gcloud run services add-iam-policy-binding soilkeypro --region us-east1 \
#       --member=allUsers --role=roles/run.invoker
set -eu
BA="${1:?billing account id required}"
PROJ=soilkeypro; REGION=us-east1; SA=budget-cap@$PROJ.iam.gserviceaccount.com
HERE="$(cd "$(dirname "$0")" && pwd)"

gcloud services enable pubsub.googleapis.com cloudfunctions.googleapis.com \
  eventarc.googleapis.com billingbudgets.googleapis.com run.googleapis.com \
  cloudbuild.googleapis.com --project $PROJ
gcloud pubsub topics describe budget-cap --project $PROJ >/dev/null 2>&1 || \
  gcloud pubsub topics create budget-cap --project $PROJ
# The budget service publishes as this Google-managed account; without the
# grant, budget notifications never reach the topic and the cap never fires.
gcloud pubsub topics add-iam-policy-binding budget-cap --project $PROJ \
  --member=serviceAccount:billing-budget-alert@system.gserviceaccount.com \
  --role=roles/pubsub.publisher >/dev/null
gcloud iam service-accounts describe $SA --project $PROJ >/dev/null 2>&1 || \
  gcloud iam service-accounts create budget-cap --project $PROJ \
    --display-name "Takes soilkeypro.com offline at the spending cap"
# Least privilege: admin on this one service, not on the project.
gcloud run services add-iam-policy-binding soilkeypro --region $REGION --project $PROJ \
  --member="serviceAccount:$SA" --role=roles/run.admin >/dev/null

gcloud functions deploy budget-cap --gen2 --project $PROJ --region $REGION \
  --runtime python312 --source "$HERE" --entry-point cap \
  --trigger-topic budget-cap --service-account "$SA" --memory 256Mi --quiet

# Eventarc delivers each message as an authenticated push using the same
# service account, so that account must be allowed to invoke the function.
# Without this every delivery fails with "The request was not authenticated"
# and the cap never fires. Do NOT allow unauthenticated invocations instead:
# that would let anyone on the internet take the site offline.
gcloud run services add-iam-policy-binding budget-cap --region $REGION --project $PROJ \
  --member="serviceAccount:$SA" --role=roles/run.invoker >/dev/null

gcloud billing budgets create --billing-account "$BA" \
  --display-name "soilkeypro: take the site offline at US\$1" \
  --budget-amount 1USD --filter-projects "projects/$PROJ" \
  --threshold-rule percent=0.5 --threshold-rule percent=1.0 \
  --notifications-rule-pubsub-topic "projects/$PROJ/topics/budget-cap"

echo "--- end-to-end test: pretend the month cost US\$2 ---"
MSG=$(printf '{"costAmount": 2.0, "budgetAmount": 1.0}')
gcloud pubsub topics publish budget-cap --project $PROJ --message "$MSG" >/dev/null
for i in $(seq 1 30); do
  code=$(curl -s -o /dev/null -w "%{http_code}" https://soilkeypro.com)
  [ "$code" = "403" ] && break; sleep 10
done
echo "site while capped: HTTP $code (403 expected)"
[ "$code" = "403" ] || { echo "CAP DID NOT FIRE -- check: gcloud functions logs read budget-cap --gen2 --region $REGION"; exit 1; }
gcloud run services add-iam-policy-binding soilkeypro --region $REGION --project $PROJ \
  --member=allUsers --role=roles/run.invoker >/dev/null
for i in $(seq 1 18); do
  code=$(curl -s -o /dev/null -w "%{http_code}" https://soilkeypro.com)
  [ "$code" = "200" ] && break; sleep 10
done
echo "site after restore: HTTP $code (200 expected)"
