#!/bin/sh
if [ -n "$IP_ADDRESS" ]; then
    echo "const URL = \"$IP_ADDRESS\";" >> ./site/address.js
elif [ -n "$SERVICE" ]; then
    IP_ADDRESS="$(kubectl get service $SERVICE --output yaml | grep 'ip:\|targetPort:' | awk -F': ' '{ print $2 }' | tac | sed 'N;s/\n/:/')"
    echo "const URL = \"$IP_ADDRESS\";" >> ./site/address.js
else
    echo 'No environment variable IP_ADDRESS nor SERVICE, client cant reach server functions'
fi
exec "$@"