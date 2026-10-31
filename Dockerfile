# Growth Forecaster container image.
# rocker/r-ver pins CRAN to a dated Posit Package Manager snapshot, so package
# versions are reproducible and installs use prebuilt Linux binaries.
FROM rocker/r-ver:4.4.3

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libpq-dev libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled --ncpus -1 \
      shiny bslib bsicons DT plotly rhandsontable readxl writexl openxlsx \
      dplyr tidyr lubridate forecast DBI RSQLite RPostgres jsonlite \
 && rm -rf /tmp/downloaded_packages

WORKDIR /app
COPY sales_forecaster.R ./
COPY R/ ./R/

# Unprivileged runtime user; /app/data holds the SQLite file when Postgres isn't configured.
RUN useradd --create-home --uid 10001 forecaster \
 && mkdir -p /app/data \
 && chown forecaster /app/data
USER forecaster

ENV GF_TZ=Asia/Kolkata
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('sales_forecaster.R', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
