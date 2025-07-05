# MeMe
MeMe is a privacy-respecting frontend to Know Your Meme

Currently this project is still in development. While it might be in a state where you can host it publicly, There is likely to be many bugs. Also, KYM constantly screws with the UI so if something breaks please tell me.

# Support
Join our [Matrix room](https://mto.vern.cc/#/#cobra-frontends:vern.cc) for support and other things related to MeMe

# Instances
See instances.json


# Run your own instance
## Dependencies
This program is written in Guile Scheme.

You will need need `guile`, `guile-gnutls`, `guile-lib`, and `guile-gcrypt`.

## Running
0. Install the dependencies
1. Clone the repository using `git clone --recurse-submodules https://git.vern.cc/cobra/MeMe`
2. Run `guile -L . meme.scm`
3. Connect to http://localhost:8003 (or point your reverse proxy to it)
4. Profit

## Environment
`PATCHES_URL` - Link to any patches that were applied. Necessary if there are any. Do not set if there aren't.

The following are optional.

`PORT` - What port to run on (default `8003`).

## Notes
When using a service manager (e.g. systemd), make sure the stop signal is set to SIGINT, or 2. This will prevent EADDRINUSE.

# Performance
## Hardware
RAM usage seems to max out below 150 MiB (max around 140MiB in production testing, baseline 107MiB).

## Page
Due to the nature and method of proxying images, the performance on first load is poor when loading a page with many images (PageSpeed Inisghts speed index of 3.3s for a meme list, but 1.2s for an article).

After the first load, though, your browser's cache control should kick in, and any subsequent requests for an image are cached. It is recommended to use a separate image cache when using a reverse proxy.

Altogether, the performance listed on PageSpeed Insights is >90 for all pages tested.

# Todo
* Asynchronously run the handler (implement stateless version of run-server)
* Speed up image proxying somehow
* Video list page*
* Video page*
* User page (it is internally linked)

\*Videos might not be added because they use embeds from stuff like tiktok and I don't know how to scrape tiktok (use existing proxitok instance?)

# Known issues
1. It's slow
2. It could probably be faster
