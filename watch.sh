while true #run indefinitely
do
  inotifywait -r -e modify,attrib,close_write,move,create,delete ./src && elm make src/Main.elm --output=elm.js
done
