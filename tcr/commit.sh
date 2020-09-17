MESSAGE=$(osascript -e 'text returned of (display dialog "eEter commit message" default answer "TCR autocommit")')
git commit -am "$MESSAGE"

