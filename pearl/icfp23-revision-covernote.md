Thanks for allowing me to revise the paper. I made the following changes:

* The paper is deanonymized, and this time I did not forget the `review` option.

* As suggested by reviewer B, and given the changes to Section 5, I removed the
  third contributions. Since a two-bullet contribution list is not pretty, I
  rephrased to a single contribtion.

* Section 5 is replaced by a much shorter section “Caveats”.

* The Datafun comparision is expanded by stressing that that’s a _total_
  language, and that helps with coming up with a denotation. This connects to
  the caveat section about the lack of a denotional semantics.

* The Hatafun comparision is expanded with example code, with even more code
  in an appendix, provided by the author. He says that he saw Hatafun mostly as
  an experiment embedding datafun's type system into Haskell, though. The
  Hatafun author approved what I wrote about this.

* Similarly, the comparision to datafix (which is at least as relevant) is
  expanded, again with the help of the author.

* The full code corresponding to the fragment in Section 3.3 is put into a new
  appendix.

* Addressed the minor comments and fixed a bunch of typos.

* Section 4.2 is more explicit the set of equations it solves.

I was not able to produce a usable PDF diff, because the tools I tried (in
particular pdf-diff) only does page-by-page comparisons, which does not work
well when the pagebreaks shift around.
