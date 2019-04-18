# Deploying into local Kubernetes cluster

If you've got a local Kubernetes cluster that you'd like to deploy Lynx to, the files here should make that easy to do via `helm`.

First, if you haven't created a Docker build, run the following from the project root:

```bash
docker build -t lynx-local .
```

Then, let's say your hostname is `dude.dev.citizennet.com`, your project directory is at `/home/dude/src/purescript-lynx`, and you'd like `make watch` to run in a container and rebuild the project upon changes. Run the following to kick off a local deployment:

```bash
helm install --name lynx .k8s/ --set ingress.host=dude.dev.citizennet.com,persistence.path=/home/dude/src/purescript-lynx,watch=true
```

And that's it! You can verify your deployment by running `kubectl get pods` and looking for `lynx-standard-web-...`. To view the logs for either, run `kubectl logs <pod name> <container name>` where `<container name>` is either `standard-web` for the http server logs, or `standard-web-watch` for the watch logs.

## Helm config options

There are a few options you can pass to `helm` via the `--set` flag when deploying:

- `ingress.host` **required**: the hostname for your local deployment, e.g. `dude.dev.citizennet.com`
- `persistence.path` **required**: the local path to your Lynx repository, e.g. `/home/dude/src/purescript-lynx`
- `watch` _optional (default: `false`)_: whether you'd like to deploy an additional container that runs `make watch` for you
