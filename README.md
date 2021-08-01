# coqToMd
Parses an annotated Coq file, and generates a corresponding markdown file.

## Build
This project requires Haskell Stack. Build the project with `stack build`.

## Use
Use coqToMd by piping in the Coq file, and piping the output to a new markdown file. E.g.

```sh
stack exec coqToMd < myCoqFile.v > myMdFile.md
```

## Translation Rules
Coq comments with an additional asterisk at the beginning are interpreted as text/markdown. coqToMd will pull the content out of the comments, and insert them directly into the markdown. E.g. `(** foo *)` is translated to `foo`.

If the comment has further asterisks, these are interpreted as headers. E.g. `(** ** foo *)` is translated as `## foo`.

Everything else is interpreted as Coq code, and enclosed by the Jeykll `{% highlight Coq %}` and `{% endhighlight %}` directives.