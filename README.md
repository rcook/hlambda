# hlambda

Example AWS Lambda function written in Haskell

## Clone repository

```
git clone https://github.com/rcook/hlambda.git
```

## Install compiler

```
stack setup
```

## Build

```
stack build --fast
```

## Run application

```
stack exec hlambda-app
```

## Run tests

```
stack test
```

## Create a zip package for upload to AWS Lambda

```
pkg
```

Note that the resulting package will contain the platform-specific binary compiled as a result of `stack build`. For this package to work, you'll need to build on an Amazon Linux-compatible version of Linux. I've found that recent 64-bit builds of Ubuntu work fine.

## Licence

Released under [MIT License][licence]

[licence]: LICENSE
