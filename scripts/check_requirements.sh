#!/usr/bin/env bash
set -euo pipefail

INSTALL_APT=0
INSTALL_R=0

for arg in "$@"; do
  case "$arg" in
    --install)
      INSTALL_APT=1
      ;;
    --install-r)
      INSTALL_R=1
      ;;
    *)
      echo "Unknown option: $arg"
      echo "Usage: $0 [--install] [--install-r]"
      exit 1
      ;;
  esac
done

missing_packages=()
missing_tools=()
missing_r_packages=()

packages=(
  git
  r-base
  r-base-dev
  curl
  rsync
  apache2
  apache2-utils
  openssl
  sqlite3
  ldap-utils
  libldap2-dev
  libsasl2-dev
  default-jdk
  build-essential
  libcurl4-openssl-dev
  libssl-dev
  libxml2-dev
)

check_missing_r_packages() {
  missing_r_packages=()
  if ! command -v Rscript >/dev/null 2>&1; then
    return
  fi

  while IFS= read -r pkg; do
    [[ -n "${pkg}" ]] && missing_r_packages+=("${pkg}")
  done < <(
    Rscript --vanilla -e '
      pkgs <- c("shiny","shinyjs","DBI","RSQLite","digest","DT","shinyWidgets","rJava","mailR","ldapr")
      missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
      if (length(missing)) writeLines(missing)
    ' 2>/dev/null || true
  )
}

for pkg in "${packages[@]}"; do
  if ! dpkg -s "$pkg" >/dev/null 2>&1; then
    missing_packages+=("$pkg")
  fi
done

for tool in git curl rsync apache2ctl sqlite3 ldapsearch java javac; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    missing_tools+=("$tool")
  fi
done

if ! command -v R >/dev/null 2>&1; then
  missing_tools+=("R")
fi
if ! command -v Rscript >/dev/null 2>&1; then
  missing_tools+=("Rscript")
fi

if [[ ! -x /opt/shiny-server/bin/shiny-server ]]; then
  missing_tools+=("shiny-server")
fi

check_missing_r_packages

echo "== Requirement check =="
if ((${#missing_packages[@]})); then
  echo "Missing packages: ${missing_packages[*]}"
else
  echo "All required packages are installed."
fi

if ((${#missing_tools[@]})); then
  echo "Missing tools: ${missing_tools[*]}"
else
  echo "All required tools are available."
fi

if command -v Rscript >/dev/null 2>&1; then
  if ((${#missing_r_packages[@]})); then
    echo "Missing R packages: ${missing_r_packages[*]}"
  else
    echo "All required R packages are installed."
  fi
else
  echo "R package check skipped (Rscript not found)."
fi

if ((INSTALL_APT == 0 && INSTALL_R == 0)); then
  echo ""
  echo "Run with --install to install missing apt packages."
  echo "Run with --install-r to install missing R packages."
  echo "Note: Shiny Server must be installed manually from the .deb package."
  exit 0
fi

if ! command -v sudo >/dev/null 2>&1 && [[ $EUID -ne 0 ]]; then
  echo "sudo is required to install packages."
  exit 1
fi

if ((INSTALL_APT == 1)); then
  if ((${#missing_packages[@]})); then
    echo "Installing missing apt packages..."
    if [[ $EUID -eq 0 ]]; then
      apt-get update
      apt-get install -y "${missing_packages[@]}"
    else
      sudo apt-get update
      sudo apt-get install -y "${missing_packages[@]}"
    fi
  else
    echo "No apt packages need installation."
  fi
fi

if ((INSTALL_R == 1)); then
  check_missing_r_packages
  if ! command -v Rscript >/dev/null 2>&1; then
    echo "Cannot install R packages: Rscript is not available."
    echo "Install R first (e.g., run with --install)."
    exit 1
  fi

  if ((${#missing_r_packages[@]})); then
    echo "Installing missing R packages..."
    if [[ $EUID -eq 0 ]]; then
      Rscript --vanilla -e \
        'install.packages(c("shiny","shinyjs","DBI","RSQLite","digest","DT","shinyWidgets","rJava","mailR","ldapr"), repos="https://cloud.r-project.org")'
    else
      sudo Rscript --vanilla -e \
        'install.packages(c("shiny","shinyjs","DBI","RSQLite","digest","DT","shinyWidgets","rJava","mailR","ldapr"), repos="https://cloud.r-project.org")'
    fi
  else
    echo "No R packages need installation."
  fi
fi

echo ""
echo "Manual steps still required:"
echo "- Install Shiny Server (.deb) if not present."
echo "- Configure Apache vhost, LDAP, and systemd env vars."
