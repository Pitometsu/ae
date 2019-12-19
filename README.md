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
cabal new-run
```
