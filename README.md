# MeMe

A privacy-respecting, JavaScript-free frontend for [Know Your Meme](https://knowyourmeme.com).

> **This is a modified fork** of [cobra/MeMe](https://codeberg.org/cobra/MeMe) by Skylar Astaroth, maintained by darlopvil since October 2026. See [`NOTICE`](NOTICE) for details. Licensed under the AGPLv3, same as upstream.

MeMe fetches pages from Know Your Meme on the server side and serves them back as plain, lightweight HTML. Your browser never talks to KYM or its CDNs: no trackers, no ads, no scripts. Images are proxied through the instance.

Know Your Meme changes its markup from time to time, which can break individual pages. Known problems and planned features are tracked in the [issue tracker](https://github.com/darlopvil/MeMe/issues).

## Usage

Replace `knowyourmeme.com` with the hostname of your MeMe instance in any KYM URL:

    https://knowyourmeme.com/memes/navy-seal-copypasta
    https://<your-instance>/memes/navy-seal-copypasta

To do this automatically for every KYM link, use a redirect extension such as [LibRedirect](https://libredirect.github.io/).

## Running your own instance

### Docker (recommended)

    git clone https://github.com/darlopvil/MeMe.git
    cd MeMe
    cp docker-compose.yml.example docker-compose.yml
    docker compose up -d --build

MeMe is now available at <http://localhost:8003>. Point your reverse proxy at it to expose it.

`docker-compose.yml` is git-ignored, so you can adapt it to your setup (networks, ports, env vars) without it ending up in a commit.

### Without Docker

MeMe is written in [Guile Scheme](https://www.gnu.org/software/guile/). Install the dependencies (Debian/Ubuntu package names shown):

    sudo apt install guile-3.0 guile-library guile-gnutls guile-gcrypt

Then run it from the repository root:

    guile meme.scm

When running under a service manager (e.g. systemd), set the stop signal to `SIGINT` (2) so the server releases its socket cleanly. Otherwise restarts can fail with `EADDRINUSE`.

## Configuration

| Variable     | Default                             | Description |
|--------------|-------------------------------------|-------------|
| `PORT`       | `8003`                              | Port to listen on. |
| `SOURCE_URL` | `https://github.com/darlopvil/MeMe` | Link shown as "Source Code" in the footer. If you run a modified version, the AGPL requires pointing it to your modified source. |

## Contributing

Bug reports and feature requests go to the [issue tracker](https://github.com/darlopvil/MeMe/issues). If a page stops rendering, include the KYM URL that fails.

## Credits and license

MeMe was created by Skylar Astaroth ([cobra/MeMe](https://codeberg.org/cobra/MeMe), originally hosted at `git.vern.cc`). This fork keeps all original copyright notices.

Licensed under the [GNU Affero General Public License v3.0 or later](LICENSE). If you run a modified version of MeMe as a network service, you must offer its source code to your users.
