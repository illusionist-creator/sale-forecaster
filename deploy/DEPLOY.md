# Deploying Growth Forecaster on your own server

This runs the app on a small Linux server in India with:

- **HTTPS** on your own domain, with certificates issued and renewed automatically (Caddy)
- **A login prompt** in front of the app (one shared username and password)
- **Postgres** for saved plan versions, with nightly backups
- **No sleep and no monthly hour limits**, unlike the shinyapps.io free tier

Expect about 30 minutes for a first setup.

## 1. Create the server

Any provider works. Two good options with Indian data centres:

| Provider | Region | Size |
|---|---|---|
| AWS Lightsail | Mumbai (ap-south-1) | 2 vCPU / 4 GB RAM |
| DigitalOcean | Bangalore (BLR1) | 2 vCPU / 4 GB RAM |

Choose **Ubuntu 24.04**. Open ports **22, 80 and 443** in the firewall. 4 GB RAM covers
a handful of simultaneous users; the ETS/ARIMA methods are the heaviest operations.

## 2. Point your domain at it

Create a DNS **A record**, e.g. `forecast.yourcompany.in`, pointing to the server's public IP.
Wait until `ping forecast.yourcompany.in` shows that IP before step 5: HTTPS certificates
can only be issued once DNS resolves.

## 3. Install Docker

```sh
ssh ubuntu@<server-ip>
curl -fsSL https://get.docker.com | sudo sh
sudo usermod -aG docker $USER && exit   # log out and back in afterwards
```

## 4. Get the code and configure it

```sh
sudo mkdir -p /opt/growth-forecaster && sudo chown $USER /opt/growth-forecaster
git clone https://github.com/illusionist-creator/sale-forecaster.git /opt/growth-forecaster
cd /opt/growth-forecaster
cp .env.example .env
openssl rand -hex 24                                                   # use as POSTGRES_PASSWORD
docker run --rm caddy:2.8 caddy hash-password --plaintext 'choose-a-strong-password'   # use as BASIC_AUTH_HASH
nano .env
```

Fill in `DOMAIN`, `POSTGRES_PASSWORD`, `BASIC_AUTH_USER` and `BASIC_AUTH_HASH`. Keep the
single quotes around the hash.

## 5. Start it

```sh
docker compose up -d --build
docker compose ps            # all three services should be "running" / "healthy"
docker compose logs -f app   # Ctrl+C to stop following
```

The first build takes 5 to 10 minutes while R packages install. Then open
`https://forecast.yourcompany.in`, sign in, and load the sample dataset to confirm everything works.

## 6. Set up backups

```sh
chmod +x deploy/backup.sh
deploy/backup.sh             # run once by hand to check it works
crontab -e                   # then add the line below
```

```
30 2 * * * /opt/growth-forecaster/deploy/backup.sh >> /var/log/forecaster-backup.log 2>&1
```

Backups land in `/opt/growth-forecaster/backups` (14 days kept). Also copy them off the
server, e.g. with `aws s3 sync` or `s3cmd sync`, because a backup on the same disk is lost
with the server.

To restore one:

```sh
gunzip -c backups/forecaster_YYYYMMDD_HHMM.sql.gz | docker compose exec -T db psql -U forecaster -d forecaster
```

## Updating to a new version

```sh
cd /opt/growth-forecaster
git pull
docker compose up -d --build
```

Saved versions live in the Postgres volume and are kept across updates.

## Changing the password

Generate a new hash (step 4), replace `BASIC_AUTH_HASH` in `.env`, then run
`docker compose up -d caddy`.

## Notes

- **User names on versions:** "Saved by" shows the login name, so if you want to know who
  saved what, give people their own logins. Add more lines inside the `basic_auth` block in
  `deploy/Caddyfile`, one `user hash` pair per person.
- **Running without Postgres:** if `GF_DB` isn't set, the app stores versions in a SQLite file
  at `data/forecaster.sqlite`. That's what happens when you run it on your own computer.
- **Managed alternative:** Posit Connect Cloud hosts Shiny apps without a server to maintain, but
  you would need to supply a hosted Postgres database for versions to persist.
