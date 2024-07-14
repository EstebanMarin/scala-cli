# Project Title

a sane environment for developing and testing 3 examples using Scala `3`, rather than Scala `2.11``

## Description

This repo is based on this [blog post](https://xebia.com/blog/streamlining-scala-development-nix-scala-cli/) that uses [this very useful tool-kit](https://typelevel.org/toolkit/)

## Getting Started

To run the code do a

```bash
❯ scala-cli sandbox.scala
Enter problem number
1. Palindrome
2. First Factorial
3. Min Window Substring
```

Enter which program you want to test

```bash
scala-cli test PalindromeProperty.test.scala
```

### Dependencies

You will need `scala-cli` [Dependency](https://scala-cli.virtuslab.org/) to run this repo
