# nonosolver
## file main running on console
1. `stack build`
2. `stack exec -- nonosolver-exe <test case>`
test cases are stored in `site/testcases` directory
<img src="./readmefiles/img1.png" width="400">

## http server
### to run
```sh
stack build
stack exec nonosolver-exe
```
### to use
1. fill in test case
2. press submit

<img src="./readmefiles/rose_input.png" width="300">

<img src="./readmefiles/rose_output.png" width="300">

<img src="./readmefiles/multi_input.png" width="300">

<img src="./readmefiles/multi_output.png" width="300">


### textbox input
The first textbox expects input of the following format: each line are row hints starting from the top row and each line hints space separated. First textbox are for rows, second are for columns.

### testcase file format
similar to the textbox input format for the first textbox, followed by a line containing only the character `e`, followed by the second textbox input and another line of `e`.

## Google Cloud Kubernetes Workload Deployment
If running docker locally, use
```Dockerfile
docker build -t nonosolver .
docker run -p 80:80 nonosolver
```