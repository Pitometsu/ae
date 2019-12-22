#### AE

Coding challenge.

##### How to start

``` shell
# In case you have no proper Cabal version
# you might consider using a Nix sandbox.

# Get Nix package manager
curl https://nixos.org/nix/install | sh
. ~/.nix-profile/etc/profile.d/nix.sh

# Open the project
cd ./ae

# Fall into development sandbox
nix-shell --pure -Q

# Build the project
cabal new-update
cabal new-build

# Run an example
cabal new-run ae -- -h
```
``` shell
Welcome! This is a a simple HTML crawler.

Usage: ae --id STRING --origin STRING --sample STRING

Available options:
  -h,--help                Show this help text
```
```shell
cabal new-run ae --\
 --id "make-everything-ok-button"\
 --origin "data/sample-0-origin.html"\
 --sample "data/sample-1-evil-gemini.html"
```
```html
"\"<a class=\\\"btn btn-success\\\" href=\\\"#check-and-ok\\\" title=\\\"Make-Button\\\" rel=\\\"done\\\" onclick=\\\"javascript:window.okDone(); return false;\\\">\\n                              Make everything OK\\n                            </a>\""
```
