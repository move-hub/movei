# movei
a move package development tool for libra move-lang.

### new package

create a new move package named `hello_word` under current dir.
```
movei new --name hello_world .
```

### check

check modules:

```
movei check
```

check script(with modules as dependencies):

```
movei check --script script1
```

### build

build modules:

```
movei build --sender 0x01
```

build scripts:


```
movei build --script script1 --sender 0x01
```

### exec

Execute script with name `say_hi`:

``` bash
movei exec --sender 0x01 -s say_hi 
```