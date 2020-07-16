# Movei

Movei a package development tool for [Move Lang](https://github.com/libra/libra/language/move-lang).

## Features

Movei provided the following common features.

- Create a project template for developing move modules.
- Check the modules.
- Build modules into move bytecodes.
- Fmt your move modules.
- Test your code. (Your can write functional tests for you modules, just like the functional-tests in Libra).

## Install

Movei is developed in Rust, not released yet, I suggest you 

build it from source:

```shell script
git clone https://github.com/move-hub/movei.git
cd movei
cargo build --release
```

Or using:

```shell script
rustup toolchain install nightly
rustup default nightly
cargo install --git https://github.com/move-hub/movei.git --branch master --bin movei
```

## Roadmap

- Make movei-fmt more cleaver.
- Make check&build faster by memorizing some intermediate compiling result. 
- Add `movei run` command to execute scripts on Libra or other move-vm based blockchain.
  It doesn't really submit transaction, just run script and get the script state change.
- Support dependent on modules developed by others, like a real package manager does.  