FROM debian:buster
RUN mkdir -p /opt/inventory-repair-app/
# ARG BINARY_PATH
WORKDIR /opt/inventory-repair-app
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libpq-dev
COPY target /opt/inventory-repair-app
# COPY static /opt/inventory-repair-app/static
COPY config /opt/inventory-repair-app/config
COPY webapps /opt/inventory-repair-app/webapps
CMD ["/opt/inventory-repair-app/inventory-repair-app"]
