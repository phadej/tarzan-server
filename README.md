# Tarzan

> Everybody stand back. I know regular expressions.

## Try out:

http://tarzanre.herokuapp.com/

## Disclaimer

- Output format resembles JavaScript to high extend, however
    - Tarzan doesn't know about greedy or non-greedy stuff (`(...)*?` etc), it treats them as the same.
    - Also Tarzan has no prefered hand, so `(fu|fuuuu)` and `(fuuu|fu)` are the same.
      - It doesn't really matter as regexp are full match by default
      - _they are (kind of) in POSIX regexps (longest match), but not in JavaScript (left-biased)_.
- Output regexp could be cleaner still.

## operator.tarzan

```js
// fn         := /[-=]>/
var fn = /^[\-=]>$/;
// assign     := /[-+*/%<>&|^!?=]=/
var assign = /^[!%&\*\+\-\/<-\?\^\|]=$/;
// zerofillrs := />>>=??/
var zerofillrs = /^>>>(?:=)?$/;
// doubles    := "--" | "++" | "::"
var doubles = /^(?:\+\+|\-\-|::)$/;
// logic      := ("&&" | "||" | "<<" | ">>") /=?/
var logic = /^(?:&&|<<|>>|\|\|)(?:=)?$/;
// soak       := "?."
var soak = /^\?\.$/;
// range      := ".." | "..."
var range = /^(?:\.\.|\.\.\.)$/;
// operator   := fn | assign | zerofillrs | doubles | logic | soak | range
var operator = /^(?:[!%&\*\+\-\/<-\?\^\|]=|\+\+|\-\-|[\-=]>|\.\.|\.\.\.|::|>>>(?:=)?|\?\.|(?:&&|<<|>>|\|\|)(?:=)?)$/;
```
