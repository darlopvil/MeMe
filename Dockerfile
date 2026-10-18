# SPDX-License-Identifier: AGPL-3.0-or-later
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        guile-3.0 \
        guile-library \
        guile-gnutls \
        guile-gcrypt \
        guile-json \
        ca-certificates \
        netbase \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . .

USER ubuntu

ENV PORT=8003
EXPOSE 8003

# SIGINT lets Guile's server release the socket cleanly (prevents EADDRINUSE)
STOPSIGNAL SIGINT

HEALTHCHECK --interval=30s --timeout=5s --start-period=15s --retries=3 \
    CMD guile -c "(use-modules (web client) (web response)) (exit (= 200 (response-code (http-get (string-append \"http://127.0.0.1:\" (or (getenv \"PORT\") \"8003\") \"/style.css\")))))"

CMD ["guile", "meme.scm"]
