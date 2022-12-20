#!/bin/sh

echo "type port to kill"

read PORT_TO_KILL
echo "========================"
echo "kill:"
echo $(lsof -i:$PORT_TO_KILL | grep LISTEN)

read -r -p  "are you sure???? [y/N] "  response
if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]
then

  echo "========================"
  echo "killing port $PORT_TO_KILL"
  kill -9 $(lsof -t -i:$PORT_TO_KILL) 

fi
# PROCESS_PID=$(lsof -i:$PORT_TO_KILL)

# echo $PROCESS_PID


