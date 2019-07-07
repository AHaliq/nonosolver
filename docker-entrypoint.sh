#!/bin/sh
if [ -n "$IP_ADDRESS" ]; then
    echo "const URL = \"$IP_ADDRESS\";" >> ./site/address.js
elif [ -n "$SERVICE" ]; then
    echo "const URL = \"$(kubectl get svc $SERVICE -o template --template '{{(index .status.loadBalancer.ingress 0).ip}}')\";" >> ./site/address.js
else
    echo 'No environment variable IP_ADDRESS nor SERVICE, client cant reach server functions'
fi
exec "$@"