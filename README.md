# nonosolver
## file main running on console
1. `stack build`
2. `stack exec -- nonosolver-exe <test case>`
test cases are stored in `site/testcases` directory
![output example](./readmefiles/img1.png)

## http server
1. fill in test case without ending with e
2. press submit
![webpage example](./readmefiles/img2.png)

### test cases
test case are formatted as from top row to bottom row each line hints for that row
partitioned with a character 'e'
then from left to right column hints
ending with a character 'e' and empty line

## Google Cloud Kubernetes Workload Deployment
Dockerfile entry points creates a js file containing ip address via `status.podIP` when run on kubernetes engine. If running docker locally, use `docker run -p 8000:8000 -e IP_ADDRESS=<address> nonosolver`.