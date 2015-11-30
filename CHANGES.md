v0.3.0 2015-11-30 Cambridge (UK)
--------------------------------

* Use the type provided by the `Result` compatibility library. Opening
  `Rresult` still gives you both unprefixed variant constructors and infix
  operators.
* Remove the `R.{int,nativeint,int32,int64,float,bool}_of_string` functions.
  They do not belong here.
* `R.map`, swap argument order. Thanks to Gabriel Radanne for suggesting.
* Fix `R.bind` which had a more restrictive type than `>>=`. Thanks to
  Hezekiah M. Carty for the patch.


v0.2.0 2015-05-20 La Forclaz (VS)
---------------------------------

* The `Rresult_infix` module no longer exists. Open directly `Rresult`
  for using the library.


v0.1.0 2015-03-19 La Forclaz (VS)
---------------------------------

First release.
