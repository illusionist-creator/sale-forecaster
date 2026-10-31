#!/usr/bin/env sh
# Nightly Postgres backup, keeping 14 days. Install with `crontab -e`:
#   30 2 * * * /opt/growth-forecaster/deploy/backup.sh >> /var/log/forecaster-backup.log 2>&1
# Copy the backups folder off the server too (e.g. to S3 or Spaces); a backup
# on the same disk does not survive losing the server.
set -eu
cd "$(dirname "$0")/.."
mkdir -p backups
stamp=$(date +%Y%m%d_%H%M)
docker compose exec -T db pg_dump -U forecaster -d forecaster | gzip > "backups/forecaster_${stamp}.sql.gz"
find backups -name 'forecaster_*.sql.gz' -mtime +14 -delete
echo "$(date -Is) backup ok: backups/forecaster_${stamp}.sql.gz"
