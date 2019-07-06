#!/bin/sh
if [ -n "$IP_ADDRESS" ]; then
    echo "const URL = \"$IP_ADDRESS\";" >> ./site/address.js
else
    echo 'No environment variable IP_ADDRESS, client cant reach server functions'
fi
exec "$@"