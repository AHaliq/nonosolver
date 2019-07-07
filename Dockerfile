FROM haskell:8

COPY --from=lachlanevenson/k8s-kubectl:v1.10.3 /usr/local/bin/kubectl /usr/local/bin/kubectl

EXPOSE 8000

RUN mkdir -p /var/www/app
COPY . /var/www/app
WORKDIR /var/www/app

RUN stack setup
RUN stack build

ENTRYPOINT [ "./docker-entrypoint.sh" ]
CMD ["stack", "run"]