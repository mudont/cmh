#!/bin/zsh
USER=$1
PASSWORD=$2
PROFILE_DATA='{ "email": "maverickone@gmail.com", "lastName": "Donthireddy", "workPhone": "212-478-0707", "username": "murali", "mobilePhone": "646-258-7065", "firstName": "Murali", "homePhone": "609-790-4750", "password": "murali12" }'
#echo $PROFILE_DATA
echo
echo "{\"username\":\"$USER\",\"password\":\"$PASSWORD\"}"
TOKEN=$(curl -X POST http://localhost:8080/login -H "Content-Type: application/json" -d "{\"username\":\"$USER\",\"password\":\"$PASSWORD\"}" |jq -r .token)
echo Got token $TOKEN
echo
echo Sleeping 0 secs
sleep 0

# Get EventRsvps for Event
#curl -X GET -H "Authorization: Bearer $TOKEN" http://localhost:8080/api/event_rsvps/495 |jq -c '.[]'
# Update My response for Event
#curl -X POST http://localhost:8080/api/event_rsvp -H "Authorization: Bearer $TOKEN" -H "Content-Type: application/json" -d "{\"eventId\":495,\"playerId\":95,\"response\":\"A\",\"comment\":\"a comm\"}" |jq -r .token
#Get EventRsvp again
curl -X GET -H "Authorization: Bearer $TOKEN" http://localhost:8080/api/event_rsvps/494 |jq 
#curl -X PUT http://localhost:8080/api/profile/murali  -H "Authorization: Bearer $TOKEN" -H "Content-Type: application/json" -d "$PROFILE_DATA" |jq
#curl -X GET -H "Authorization: Bearer $TOKEN" http://localhost:8080/api/profile/murali |jq
#curl -X GET -H "Authorization: Bearer $TOKEN" http://localhost:8080/api/user/murali |jq -c '.[]'
#
# Get players
#curl -X GET -H "Authorization: Bearer $TOKEN" http://localhost:8080/api/players |jq -c '.[]'
#curl -X GET -H "Authorization: Bearer $TOKEN" http://localhost:8080/api/events |jq -c '.[]'
#curl -X GET -H "Authorization: Bearer $TOKEN" http://localhost:8080/api/chuck
#echo
#curl -X GET http://localhost:8080/api/rajni
#curl -X GET "http://localhost:8080/reset_password?email=maverickone%40gmail.com"
#echo
