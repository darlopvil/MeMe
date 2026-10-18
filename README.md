# MeMe

A privacy-respecting, JavaScript-free frontend for [Know Your Meme](https://knowyourmeme.com).

> **This is a modified fork** of [cobra/MeMe](https://codeberg.org/cobra/MeMe) by Skylar Astaroth, maintained by darlopvil since October 2026. See [`NOTICE`](NOTICE) for details. Licensed under the AGPLv3, same as upstream.

MeMe fetches pages from Know Your Meme on the server side and serves them back as plain, lightweight HTML. Your browser never talks to KYM or its CDNs: no trackers, no ads, no scripts. Images are proxied through the instance.

Know Your Meme changes its markup from time to time, which can break individual pages. Known problems and planned features are tracked in the [issue tracker](https://github.com/darlopvil/MeMe/issues).

## What this fork adds

Compared to upstream (cobra/MeMe at `4b3b282`), this fork covers most of the site and fixes the pages that had stopped working.

**Pages**

- Search for every context (entries, images, videos, news, editorials, users), with KYM's sort options and pagination.
- Editorials and news: listings and full articles. Also KYM's episodes.
- Video listings and single video pages, with previous / next links.
- User profiles, with links to each user's images and videos.
- Comments on entries, images, videos and articles, rendered on the server in batches of 20, with nested replies and collapsed sensitive comments.
- Entry pages show related entries, sub-entries and strips of recent images and videos.
- Image and video pages show who uploaded them. Image pages also show their tags and KYM's notes.
- Random entry, image and video.
- Complete navigation bar: confirmed, submissions, trending, image sorting, episodes, news and a search box.

**Privacy**

- Embeds are served by self-hosted frontends: TikToks by ProxiTok, tweets and video tweets by Nitter, YouTube videos by Invidious, Imgur images by rimgo and Tumblr images by Priviblur.
- Links to Twitter/X, YouTube, Reddit, TikTok, Imgur, Tenor and Tumblr point at the configured frontends.
- Images from KYM's CDNs, YouTube thumbnails and Tenor GIFs go through the instance's image proxy. Any other third-party content becomes a labelled link instead of being loaded. KYM's Google Trends widget becomes a plain link.
- User-written comment HTML is reduced to a whitelist of safe elements and attributes.
- A `robots.txt` asks crawlers not to index the instance.

**Reliability**

- Scrapers select elements by class instead of fixed positions, so extra wrapper elements in KYM's markup no longer break pages.
- Requests are served concurrently: one thread per connection, at most 32 at once, and a 504 page after 60 seconds if KYM does not answer.
- Layout changes and upstream failures produce readable error pages instead of bare 500s. A KYM outage no longer makes open tabs reload every second.
- Upstream connections are always closed. The image proxy percent-encodes URLs, follows redirects internally and upgrades plain http.
- Redirects keep their query string, and paths under `/sensitive/` work.
- The server exits immediately on SIGINT or SIGTERM, so `docker stop` does not wait for the kill timeout.
- Each request is logged as a single `key=value` line, with its error if any.

**Deployment**

- Dockerfile building from the checked-out source (non-root user, healthcheck) and a `docker-compose.yml.example`.
- The footer's source link points to the code actually running, as the AGPL requires (`SOURCE_URL`), and `NOTICE` records the modifications.

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

    sudo apt install guile-3.0 guile-library guile-gnutls guile-gcrypt guile-json

Then run it from the repository root:

    guile meme.scm

MeMe exits immediately on `SIGINT` or `SIGTERM`, so it can run under any service manager (e.g. systemd) with the default stop signal.

## Configuration

| Variable     | Default                             | Description |
|--------------|-------------------------------------|-------------|
| `PORT`       | `8003`                              | Port to listen on. |
| `SOURCE_URL` | `https://github.com/darlopvil/MeMe` | Link shown as "Source Code" in the footer. If you run a modified version, the AGPL requires pointing it to your modified source. |
| `RIMGO_URL`     | unset | [rimgo](https://codeberg.org/rimgo/rimgo) instance used to display Imgur images. |
| `PROXITOK_URL`  | unset | [ProxiTok](https://github.com/pablouser1/ProxiTok) instance used to embed TikTok videos. |
| `NITTER_URL`    | unset | [Nitter](https://github.com/zedeus/nitter) instance used to embed tweets. |
| `INVIDIOUS_URL` | unset | [Invidious](https://github.com/iv-org/invidious) instance used to embed YouTube videos. |
| `EDDRIT_URL`    | unset | [eddrit](https://github.com/corenting/eddrit) instance that Reddit links point to. |
| `SOPRANO_URL`   | unset | [Soprano](https://codeberg.org/cobra/Soprano) instance that Tenor links point to. |
| `PRIVIBLUR_URL` | unset | [Priviblur](https://github.com/syeopite/priviblur) instance used for Tumblr links and images. |

Links to those sites inside entries, articles, profiles and comments are also rewritten to point at the configured frontend. When one of the frontend variables is unset, that kind of embed is shown as a plain link to the original site instead, so the browser never loads anything from Imgur, TikTok, Twitter/X, YouTube or Tumblr on its own. Embedded frontends must allow being framed (no `X-Frame-Options` or restrictive `frame-ancestors` on their embed routes).

## Contributing

Bug reports and feature requests go to the [issue tracker](https://github.com/darlopvil/MeMe/issues). If a page stops rendering, include the KYM URL that fails.

## Credits and license

MeMe was created by Skylar Astaroth ([cobra/MeMe](https://codeberg.org/cobra/MeMe), originally hosted at `git.vern.cc`). This fork keeps all original copyright notices.

Licensed under the [GNU Affero General Public License v3.0 or later](LICENSE). If you run a modified version of MeMe as a network service, you must offer its source code to your users.
