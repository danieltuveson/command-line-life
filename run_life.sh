HEIGHT="$(tput lines)"
WIDTH="$(tput cols)"
FRAMERATE=10
cabal new-run command-line-life $WIDTH $HEIGHT $FRAMERATE
