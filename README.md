# Haskell bindings for Gumbo

Gumbo is a standard-compliant HTML5 parser from Google. For example,

```hs
parse "<p><i>hello <hr>world"
```

correctly duplicates the `i` element:

```html
<p>
› <i>
› › hello
› </i>
</p>
<hr>
<i>
› world
</i>
```

`parse` runs the C parser, then copies the entire DOM tree into the Haskell heap; the memory used by Gumbo is freed immediately.

`parseLazy` parses up front, but reifies the DOM nodes as they are accessed.

There's also an `(html :: Lens' BS Node)` if you're into that sort of thing.
