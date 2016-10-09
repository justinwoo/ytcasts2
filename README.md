# ytcasts2

a haskell rewrite of
[ytcasts](https://github.com/justinwoo/ytcasts).

## Setup

`stack install`

add `config.json` with the following format:

```js
{
  "targets": [
    "https://www.youtube.com/user/USER/videos",
    ...
  ]
}
```

and add a `data` sqlite file with the schema in `schema.sql`.

then run `ytcasts2-exe` in this directory.
