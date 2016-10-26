Workflow for using purescript-pux-devtools
==========================================
```
> cd path/to/gonimo-back
> stack exec -- gonimo-back&
> cd path/to/gonimo-front
> bower update
> rm -rf ./bower_components/purescript-newtype
> npm install
> npm start
> chromium-browser --disable-web-security --user-data-dir
```
~> `localhost:3000` in chromium

> Note that if your chromium-browser is single instance only - you should not go
> to any suspicious site (not that you should go there anytime else).

Workflow for deployment
-----------------------

```
> cd path/to/gonimo-front
> bower update
> rm -rf ./bower_components/purescript-newtype
> pulp browserify --to dist/app.js && cp -R ./static/* dist

the removal of `purescript-newtype` should only be necessary temporarily as this
is either fixed by checking the `"resolutions"` section in `bower.json`, or by
the migration to purescript-0.9.3 to purescript-0.10.x.


