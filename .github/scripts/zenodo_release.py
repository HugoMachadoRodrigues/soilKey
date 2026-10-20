"""Archive a soilKey release on Zenodo as a new version of the concept record
10.5281/zenodo.19930112. Called by .github/workflows/zenodo.yaml.

Environment: ZENODO_TOKEN (deposit:write and deposit:actions scopes), TAG,
ZIP (the git archive of the tag), RELEASE_DATE (YYYY-MM-DD), DRY_RUN
("true" only reads: token, latest version, metadata), GITHUB_REPOSITORY.
"""
import json
import os
import sys
import urllib.error
import urllib.request

API = "https://zenodo.org/api"
CONCEPT = "19930112"


def call(method, url, data=None, ctype="application/json", auth=True):
    req = urllib.request.Request(url, data=data, method=method)
    if auth:
        req.add_header("Authorization", "Bearer " + os.environ["ZENODO_TOKEN"])
    if data is not None:
        req.add_header("Content-Type", ctype)
    try:
        with urllib.request.urlopen(req, timeout=600) as r:
            body = r.read()
            return r.status, (json.loads(body) if body else {})
    except urllib.error.HTTPError as e:
        return e.code, {"error": e.read().decode("utf-8", "replace")[:800]}


def need(status, body, ok, what):
    if status not in ok:
        sys.exit("%s failed: HTTP %s %s" % (what, status, body.get("error", "")))


def archived_versions(latest):
    """Version strings already on Zenodo under the concept record."""
    seen, page = [], 1
    while True:
        s, d = call("GET", "%s/records/%s/versions?size=25&page=%d&allversions=true"
                    % (API, latest, page), auth=False)
        need(s, d, (200,), "listing the versions")
        hits = d["hits"]["hits"]
        seen += [h["metadata"].get("version") for h in hits]
        if not hits or len(seen) >= d["hits"]["total"]:
            return seen
        page += 1


def metadata(tag, date):
    with open(".zenodo.json", encoding="utf-8") as fh:
        md = json.load(fh)
    md["version"] = tag
    md["publication_date"] = date
    md.setdefault("related_identifiers", []).append({
        "scheme": "url",
        "identifier": "https://github.com/%s/tree/%s" % (os.environ["GITHUB_REPOSITORY"], tag),
        "relation": "isSupplementTo",
        "resource_type": "software"})
    return md


def open_draft(latest):
    """A new-version draft of the latest record, or the draft a failed run left."""
    s, d = call("POST", "%s/deposit/depositions/%s/actions/newversion" % (API, latest))
    if s in (200, 201):
        return d["links"]["latest_draft"]
    s2, drafts = call("GET", "%s/deposit/depositions?status=draft&size=100" % API)
    need(s2, drafts if isinstance(drafts, dict) else {}, (200,), "listing the drafts")
    mine = [x for x in drafts if str(x.get("conceptrecid")) == CONCEPT]
    if len(mine) != 1:
        sys.exit("new version failed: HTTP %s %s" % (s, d.get("error", "")))
    print("Reusing the draft %s left by an earlier run" % mine[0]["id"])
    return mine[0]["links"]["self"]


def main():
    tag, date, zip_path = os.environ["TAG"], os.environ["RELEASE_DATE"], os.environ["ZIP"]
    dry = os.environ.get("DRY_RUN", "false") == "true"

    s, rec = call("GET", "%s/records/%s" % (API, CONCEPT), auth=False)
    need(s, rec, (200,), "resolving the concept record")
    latest = rec["id"]
    print("Latest version on Zenodo: %s (%s)" % (rec["metadata"].get("version"), rec["doi"]))

    s, dep = call("GET", "%s/deposit/depositions/%s" % (API, latest))
    need(s, dep, (200,), "reading the latest deposition with the token")
    print("The token works and owns the record.")

    if tag in archived_versions(latest):
        print("%s is already on Zenodo; nothing to do." % tag)
        return

    md = metadata(tag, date)
    print("Zip: %s (%d bytes)" % (zip_path, os.path.getsize(zip_path)))
    print(json.dumps(md, indent=1, ensure_ascii=False))
    if dry:
        print("Dry run: the token works and nothing was created.")
        return

    draft_url = open_draft(latest)
    s, draft = call("GET", draft_url)
    need(s, draft, (200,), "reading the draft")
    for f in draft.get("files", []):
        s, d = call("DELETE", "%s/files/%s" % (draft_url, f["id"]))
        need(s, d, (204,), "removing the inherited file " + f.get("filename", ""))
    name = os.path.basename(zip_path)
    with open(zip_path, "rb") as fh:
        s, d = call("PUT", "%s/%s" % (draft["links"]["bucket"], name), fh.read(),
                    "application/octet-stream")
    need(s, d, (200, 201), "uploading " + name)
    s, d = call("PUT", draft_url, json.dumps({"metadata": md}).encode())
    need(s, d, (200,), "writing the metadata")
    s, d = call("POST", draft_url + "/actions/publish")
    need(s, d, (202,), "publishing")

    line = "%s archived on Zenodo: https://doi.org/%s (concept https://doi.org/10.5281/zenodo.%s)" % (
        tag, d["doi"], CONCEPT)
    print(line)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8") as fh:
            fh.write(line + "\n")


if __name__ == "__main__":
    main()
