```
nix-env -iA cabal-install -f '<nixpkgs>'
nix-shell -A env
cabal configure --ghcjs
cabal repl
mkdir -p static
closure-compiler result/bin/app.jsexe/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars > static/all.min.js
```