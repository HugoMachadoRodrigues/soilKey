# Deploying soilKey Pro to Fly.io (soilkeypro.com)

The public Shiny app is a container defined by [`Dockerfile`](Dockerfile) and
configured by [`fly.toml`](fly.toml). It is **not** the repo-root `Dockerfile`
(that one is a reproducible R REPL image); this image installs the full Pro-app
runtime dependency set and launches the app on `0.0.0.0:8080`.

## One-time setup

```sh
# 1. Install the CLI (macOS)
brew install flyctl

# 2. Sign in (opens a browser; you approve there)
fly auth login

# 3. Create the app WITHOUT deploying yet (context = repo root)
fly launch --config deploy/fly/fly.toml --no-deploy --copy-config --name soilkeypro
```

## Deploy (build happens remotely on Fly's amd64 builder)

```sh
fly deploy --config deploy/fly/fly.toml .
fly logs   --config deploy/fly/fly.toml      # watch it boot
fly open   --config deploy/fly/fly.toml      # opens https://soilkeypro.fly.dev
```

## Custom domain + TLS (soilkeypro.com)

DNS is managed at Cloudflare. Add the records Fly asks for, then:

```sh
fly certs add soilkeypro.com     --config deploy/fly/fly.toml
fly certs add www.soilkeypro.com --config deploy/fly/fly.toml
fly certs show soilkeypro.com    --config deploy/fly/fly.toml   # check status
```

In the Cloudflare dashboard (DNS for soilkeypro.com), create:

| Type  | Name | Value                          | Proxy        |
|-------|------|--------------------------------|--------------|
| A     | `@`  | Fly's IPv4 (from `fly ips list`) | DNS only (grey) |
| AAAA  | `@`  | Fly's IPv6 (from `fly ips list`) | DNS only (grey) |

Use **DNS only** (grey cloud) so Fly's own Let's Encrypt cert can be issued.
Once the cert is `Ready`, you may optionally switch Cloudflare back to
**Proxied** with SSL mode **Full (strict)**.

## Cost / scaling

Defaults: one always-on `shared-cpu-1x` machine with 1 GB RAM (~US$5-6/mo).
To cut cost with scale-to-zero (adds a cold start on the first visit), set in
`fly.toml`: `auto_stop_machines = "stop"` and `min_machines_running = 0`.
Bump `[[vm]] memory` to `2gb` if the app OOMs under load (`fly logs`).
