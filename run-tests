#!/bin/bash
# Test API endpoints with a running server

set -e

echo "Testing API endpoints..."

# Test basic operations
echo "1. Testing GET /show (initial value)"
curl -s http://localhost:8080/show | jq .

echo -e "\n2. Testing POST /add with value 15"
curl -s -X POST -H "Content-Type: application/json" -d '{"value": 15}' http://localhost:8080/add | jq .

echo -e "\n3. Testing GET /show (after addition)"
curl -s http://localhost:8080/show | jq .

echo -e "\n4. Testing POST /add with value 7"
curl -s -X POST -H "Content-Type: application/json" -d '{"value": 7}' http://localhost:8080/add | jq .

echo -e "\n5. Testing GET /show (final value)"
curl -s http://localhost:8080/show | jq .

echo -e "\n6. Testing DELETE /reset"
curl -s -X DELETE http://localhost:8080/reset | jq .

echo -e "\n7. Testing GET /show (after reset)"
curl -s http://localhost:8080/show | jq .

echo -e "\nAPI testing completed!"