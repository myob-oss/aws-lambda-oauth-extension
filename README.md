# aws-lambda-oauth-extension

caching and oauth client credentials layer for lambda

[![](https://docs.aws.amazon.com/lambda/latest/dg/images/Overview-Full-Sequence.png)][3]

1. the extension will start a http server in `init` phase at port `8081`
2. in invoke phase, function can send request to `localhost:8081` with header `Host: example.com`, the request will automatically authenticated then proxy to `example.com`
3. extension turns off the server when receiving `shutdown` event

## :space_invader:Install
example: add extension to a Python image
```
FROM public.ecr.aws/lambda/python:3.8

# Copy and install the app
COPY /app /app
WORKDIR /app
RUN pip install -r requirements.txt

# extract the extension into root folder
ADD extension.tar.gz /
CMD python ./index.py
```

## :gear: Config
config can be add as file or inline through environment variables `ALOE_CONFIG`
### File
1. Add config file to image
```
...
# app config into image
ADD config.dhall /opt/extensions/
# extract the extension into root folder
ADD extension.tar.gz /
...
```
2. add env to lambda `ALOE_CONFIG=/opt/extensions/config.dhall`

### Inline Dhall
in `template.dhall`
```
Environment = Some Funtion.Environment::{
  Variables = Some (toMap {
    ALOE_CONFIG = ./config.dhall as Text
  })
}
```

## :unicorn: Contribute

### Prerequisites
1. [nix for macOS or Linux](https://nixos.org/download.html#nix-quick-install)
```
curl https://nixos.org/nix/install | sh
```
2. [stack](https://github.com/commercialhaskell/stack)
```
nix-env -i stack
```
### Build

```
stack build
```

### Package
#### binary
```
nix-build
```
binary will be generated in `./result/bin/aws-lambda-oauth-extension-exe`

#### docker
- on Linux
```
docker load < $(nix-build docker.nix)
```
docker image is tagged `ghcr.io/myob-technology/aws-lambda-oauth-extension:latest`
- on macOS
```
docker-compose run --rm build-image
docker load < image.gz
```

### Test
```
nix-shell --run 'bats test'
```

## Ref

[1]: https://github.com/aws-samples/aws-lambda-extensions/tree/main/cache-extension-demo
[2]: https://aws.amazon.com/blogs/compute/working-with-lambda-layers-and-extensions-in-container-images/
[3]: https://docs.aws.amazon.com/lambda/latest/dg/runtimes-extensions-api.html
