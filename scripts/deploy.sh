#!/bin/bash

set -euo pipefail

########################################################################################
# This script will do the following steps to ensure the smooth running of the app.
# #
# 1. syncs the app code to the Shiny Server directory using rsync:
#     - Source: /home/yeroslaviz/BCFNGS-project-management/sequencing-app/
#     - Target: /srv/shiny-server/sequencing-app/
#     - Uses --delete to remove files in the target that no longer exist in the source.
#     - Excludes sequencing_projects.db so the production DB is not overwritten.
#     - Excludes .Renviron so VM runtime auth/env settings are preserved.
#
# 2. Fixes ownership of the deployed folder:
#
#     - shiny:shiny on everything under /srv/shiny-server/sequencing-app
#
# 3. Makes the DB writable:
#    - sequencing_projects.db
#
# 4. Restarts Shiny Server
#
# 5. Runs LDAP smoke tests (unless SKIP_SMOKE_TEST=1):
#    - expects 302 canonicalization to ?auth_user=<authenticated user>
#    - expects tampered auth_user to be rewritten
#    - expects final 200 after redirects
########################################################################################


APP_SOURCE="/home/yeroslaviz/BCFNGS-project-management/sequencing-app/"
APP_TARGET="/srv/shiny-server/sequencing-app/"
APP_DB="${APP_TARGET}sequencing_projects.db"
PUBLIC_BASE_URL="${PUBLIC_BASE_URL:-https://ngs-testing-vm.biochem.mpg.de}"
APP_PATH="/sequencing-app/"
APP_URL="${PUBLIC_BASE_URL%/}${APP_PATH}"

smoke_fail() {
  local message="$1"
  echo
  echo "LDAP smoke test failed: ${message}" >&2
  echo "Suggested checks:" >&2
  echo "1) Verify the active Apache vhost for ${APP_URL} serves /sequencing-app/ with LDAP auth:" >&2
  echo "   sudo apache2ctl -S" >&2
  echo "   sudo apache2ctl configtest && sudo systemctl restart apache2 && sudo systemctl restart shiny-server" >&2
  echo "2) Verify Shiny environment:" >&2
  echo "   sudo systemctl show shiny-server --property=Environment" >&2
  echo "   Required: AUTH_MODE=ldap and TRUST_PROXY_AUTH_USER_QUERY=1" >&2
  echo "3) Inspect logs for redirects/auth failures:" >&2
  echo "   sudo tail -n 120 /var/log/apache2/sequencing-app-ssl-access.log" >&2
  echo "   sudo tail -n 120 /var/log/apache2/sequencing-app-ssl-error.log" >&2
  exit 1
}

extract_http_code() {
  awk '/^HTTP/{code=$2} END{print code}'
}

extract_location() {
  awk 'BEGIN{IGNORECASE=1}
       /^Location:[[:space:]]/ {
         sub(/\r$/, "", $0)
         sub(/^Location:[[:space:]]*/, "", $0)
         print
         exit
       }'
}

run_ldap_smoke_tests() {
  if [ "${SKIP_SMOKE_TEST:-0}" = "1" ]; then
    echo "Skipping LDAP smoke tests (SKIP_SMOKE_TEST=1)."
    return
  fi

  echo
  echo "Running LDAP smoke tests..."

  local env_line
  env_line="$(sudo systemctl show shiny-server --property=Environment)"
  if [[ "${env_line}" != *"AUTH_MODE=ldap"* ]]; then
    smoke_fail "AUTH_MODE=ldap is not set in shiny-server environment."
  fi
  if [[ "${env_line}" != *"TRUST_PROXY_AUTH_USER_QUERY=1"* ]]; then
    smoke_fail "TRUST_PROXY_AUTH_USER_QUERY=1 is not set in shiny-server environment."
  fi

  local ldap_user="${LDAP_TEST_USER:-}"
  local ldap_password="${LDAP_TEST_PASSWORD:-}"
  local tampered_user="${TAMPERED_AUTH_USER:-yeroslaviz}"
  local headers status location expected_location final_status
  local netrc_file app_host

  if [ -z "${ldap_user}" ]; then
    read -r -p "LDAP smoke test username [yeroslaviz-test]: " ldap_user
    ldap_user="${ldap_user:-yeroslaviz-test}"
  fi

  if [ -z "${ldap_password}" ]; then
    read -r -s -p "LDAP password for ${ldap_user}: " ldap_password
    echo
  fi
  if [ -z "${ldap_password}" ]; then
    smoke_fail "No LDAP password provided."
  fi

  if [ "${tampered_user}" = "${ldap_user}" ]; then
    tampered_user="${ldap_user}-tampered"
  fi

  app_host="$(printf "%s" "${APP_URL}" | sed -E 's#^https?://([^/]+)/?.*#\1#')"
  netrc_file="$(mktemp)"
  chmod 600 "${netrc_file}"
  cat > "${netrc_file}" <<EOF
machine ${app_host}
login ${ldap_user}
password ${ldap_password}
EOF
  trap 'rm -f "${netrc_file}"' EXIT

  expected_location="${APP_URL}?auth_user=${ldap_user}"

  headers="$(curl -k -sS -D - -o /dev/null --netrc-file "${netrc_file}" "${APP_URL}")"
  status="$(printf "%s\n" "${headers}" | extract_http_code)"
  location="$(printf "%s\n" "${headers}" | extract_location)"
  if [ "${status}" != "302" ] || [ "${location}" != "${expected_location}" ]; then
    smoke_fail "Expected 302 + Location=${expected_location} for ${APP_URL}, got status=${status}, location=${location:-<missing>}."
  fi

  headers="$(curl -k -sS -D - -o /dev/null --netrc-file "${netrc_file}" "${APP_URL}?auth_user=${tampered_user}")"
  status="$(printf "%s\n" "${headers}" | extract_http_code)"
  location="$(printf "%s\n" "${headers}" | extract_location)"
  if [ "${status}" != "302" ] || [ "${location}" != "${expected_location}" ]; then
    smoke_fail "Tampered auth_user was not canonicalized. Expected 302 + Location=${expected_location}, got status=${status}, location=${location:-<missing>}."
  fi

  final_status="$(curl -k -sS -L -o /dev/null -w "%{http_code}" --netrc-file "${netrc_file}" "${APP_URL}")"
  if [ "${final_status}" != "200" ]; then
    smoke_fail "Final app response after redirects is ${final_status} (expected 200)."
  fi

  rm -f "${netrc_file}"
  trap - EXIT
  echo "LDAP smoke tests passed."
}

echo "Deploying Shiny app..."

if sudo rsync -av --delete --exclude 'sequencing_projects.db' --exclude '.Renviron' "${APP_SOURCE}" "${APP_TARGET}"; then
  sudo chown -R shiny:shiny "${APP_TARGET}"
  if [ -f "${APP_DB}" ]; then
    sudo chmod 666 "${APP_DB}"
  else
    echo "Note: DB file not found at ${APP_DB} (skipping chmod)."
  fi
  sudo systemctl restart shiny-server
  echo "Deployment complete."
  run_ldap_smoke_tests
else
  echo "Deploy failed: rsync error. Shiny server was NOT restarted."
  exit 1
fi
