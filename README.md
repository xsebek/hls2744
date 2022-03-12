# HLS 2744

This is the example given in [Haskell language server issue #2744](https://github.com/haskell/haskell-language-server/issues/2744).

It depends on [hmatrix-glpk](https://hackage.haskell.org/package/hmatrix-glpk), so you need to install some system dependencies.

To install gsl, lapack, atlas and glpk run:
- on Fedora:
  ```bash
  sudo dnf install gsl-devel lapack-devel atlas-devel glpk-devel
  ```