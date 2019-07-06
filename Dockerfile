FROM haskell:8

EXPOSE 8000

RUN mkdir -p /var/www/app
COPY . /var/www/app
WORKDIR /var/www/app

RUN stack setup
RUN stack build

ENTRYPOINT [ "./docker-entrypoint.sh" ]
CMD ["stack", "run"]