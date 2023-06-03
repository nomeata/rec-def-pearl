# Datafun

This is an implementation of Datafun in Haskell with high-order abstract syntax (HOAS) and tagless-final. In particular, the monotonic type system presented in the original paper is embedded into Haskell. For techniques behind, see the references.

## Examples

There are three tiny examples that are currently included: computing transitive closures for finite semilattices, computing transitive closures for infinite semilattices with an upper bound, and CKY parsing algorithm for Context-Free Grammars.

## References

* Michael Arntzenius and Neelakantan R. Krishnaswami. 2016. Datafun: a functional Datalog. In Proceedings of the 21st ACM SIGPLAN International Conference on Functional Programming (ICFP 2016). Association for Computing Machinery, New York, NY, USA, 214–227. DOI:https://doi.org/10.1145/2951913.2951948
* Jeff Polakow. 2015. Embedding a full linear Lambda calculus in Haskell. In Proceedings of the 2015 ACM SIGPLAN Symposium on Haskell (Haskell ’15). Association for Computing Machinery, New York, NY, USA, 177–188. DOI:https://doi.org/10.1145/2804302.2804309
* http://okmij.org/ftp/tagless-final/course/LinearLC.hs