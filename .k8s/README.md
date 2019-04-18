# Deploying into local Kubernetes cluster

If you've got a local Kubernetes cluster that you'd like to deploy Lynx to, the files here should make that easy to do via `helm`.

First, if you haven't created a Docker build, run the following from the project root:

```bash
docker build -t lynx-local .
```

With that out of the way, run the following to kick off a local deployment:

```bash
helm install --name lynx .k8s/ --set ingress.host=$(hostname),persistence.path=/home/$USER/src/purescript-lynx,watch=true
```

If you don't have `hostname` configured or your project repo is not set up in `~/src`, then you'll have to manually substitute the value for `ingress.host` with your hostname, and the value for `persistence.path` with your actual project location.

And that's it! You can verify your deployment by running `kubectl get pods` and looking for `lynx-standard-web-...`. To view the logs for either, run `kubectl logs <pod name> <container name>` where `<container name>` is either `standard-web` for the http server logs, or `standard-web-watch` for the watch logs.

## Helm config options

There are a few options you can pass to `helm` via the `--set` flag when deploying:

- `ingress.host` **required**: the hostname for your local deployment, e.g. `dude.dev.citizennet.com`
- `persistence.path` **required**: the local path to your Lynx repository, e.g. `/home/dude/src/purescript-lynx`
- `watch` _optional (default: `false`)_: whether you'd like to deploy an additional container that runs `make watch` for you
