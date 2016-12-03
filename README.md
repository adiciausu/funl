# funl
Testing:

siege -c 10 -r 100000 -b http://127.0.0.1:8080/redirect
siege -c 10 -r 100000 -b http://127.0.0.1:8080/ok
siege -c 10 -r 100000 -b http://127.0.0.1:8080/fail


