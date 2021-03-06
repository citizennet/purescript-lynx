FROM node:stretch

# Global install yarn package manager
# Global install http-server
RUN apt-get update && apt-get install -y curl apt-transport-https && \
    curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - && \
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
    apt-get update && apt-get install -y yarn && \
    yarn global add http-server

RUN mkdir -p /usr/src/app

COPY . /usr/src/app

WORKDIR /usr/src/app

ENTRYPOINT make && http-server ./dist -p 80
