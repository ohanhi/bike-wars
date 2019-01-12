while true #run indefinitely
do
	inotifywait -r -e modify,attrib,close_write,move,create,delete ./src && ./node_modules/.bin/elm-make src/Main.elm --output=/dev/null
done
