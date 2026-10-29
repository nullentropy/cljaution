# cljaution

The Clojure server SDK for caution. A view is a pure function from session
state to a hiccup tree; the server diffs trees and sends patches to the
caution terminal (browser or native).

    clj -M:demo     # the demo app on :8787
    clj -M:test     # tests

## Terminal assets

`resources/caution/app.js` and `resources/caution/term.wasm.gz` are copies
of the terminal built in the caution repo. After rebundling there, refresh
them:

    cp ../caution/embedded/app.js ../caution/embedded/term.wasm.gz resources/caution/

During development the server prefers `../caution/dist/app.js` when it
exists (override with `:app-js`).
