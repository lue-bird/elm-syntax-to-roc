Transpile all elm modules in the current project
(source-directories + dependencies)
into a bundled `src/Elm.roc` file that exposes every value/function declaration
(e.g. `mainRunOnString` for `Main.runOnString`)


```bash
npm install && npm run build
```

To instead run it once

```bash
npm run start
```
