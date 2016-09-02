In this article we'll talk about installing a Haskell toolchain on your computer.

Installing a Haskell toolchain becomes really easy when you use [Stack](https://haskellstack.org).

### Stack

Stack is a Haskell build tool that is used to build Haskell software, manage package
dependencies and even installing the Glorious Haskell Compiler (AKA GHC).

To get started, go ahead and [download Stack](https://haskell-lang.org/get-started).

After installing task you'll be able to execute it by running the `stack` command in
your shell. Let's verify we indeed installed Stack correctly:

```
suppi@ubuntu:~$ stack --version
Version 1.1.2, Git revision cebe10e845fed4420b6224d97dcabf20477bbd4b (3646 commits) x86_64 
hpack-0.14.0
```

Alright! Looks like it's working!

### Installing GHC

The next thing we might want to do is install the GHC compiler. We can use stack to install
it for us. To do that, all we need to write is:

```
suppi@ubuntu:~$ stack setup
Run from outside a project, using implicit global project config
Using latest snapshot resolver: lts-6.14
Writing implicit global project config file to: /home/suppi/.stack/global-project/stack.yaml
Note: You can change the snapshot via the resolver field there.
Downloaded lts-6.14 build plan.    
Populated index cache.    
Did not find .cabal file for snaplet-fay-0.3.3.13 with Git SHA of 
423420d2e322320fec411d95c2cbd3a2f5c45991
Right Nothing
Did not find .cabal file for rest-snap-0.2.0.1 with Git SHA of 
5e7a306f05d1f04dba00d4b06da8f9e501a1e49e
Right Nothing
Did not find .cabal file for aeson-better-errors-0.9.0.1 with Git SHA of 
0deb275b4df73c2a7a042d2e7226eb1b23de8846
Right Nothing
Did not find .cabal file for Spock-worker-0.3.0.0 with Git SHA of 
dfdf645795042f1dab44aff29564c8ce82a52c8d
Right Nothing
Did not find .cabal file for Spock-digestive-0.2.0.0 with Git SHA of 
df169ccab1012421a82bfc5e65d42e406eb9ed63
Right Nothing
stack will use a locally installed GHC
For more information on paths, see 'stack path' and 'stack exec env'
To use this GHC and packages outside of a project, consider using:
stack ghc, stack ghci, stack runghc, or stack exec
```

After that, the you'll be able to run `ghc`, `runghc` and `ghci` anywhere by typing

```
stack exec <command>
```

and if you need to supply arguments to `ghc`, `runghc` or `ghci`, type `--` before your argument list:

```
stack exec <command> -- ARGS
```

For example:

```
suppi@ubuntu:~$ stack exec ghc -- --version
Run from outside a project, using implicit global project config
Using resolver: lts-6.14 from implicit global project's config file: 
/home/suppi/.stack/global-project/stack.yaml
The Glorious Glasgow Haskell Compilation System, version 7.10.3
```

### Playing with Haskell

Let's try opening `ghci` and write something

```
suppi@ubuntu:~$ stack exec ghci
Run from outside a project, using implicit global project config
Using resolver: lts-6.14 from implicit global project's config file: 
/home/suppi/.stack/global-project/stack.yaml
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> putStrLn "Hello, Haskell!"
Hello, Haskell!
Prelude> :q
Leaving GHCi.
```

Ok! so Stack ran GHCi using the global project configuration.
Stack can manage different dependencies for different projects, so when you are
not in a folder associated with a project, it will use the default configuration.

Next, we write some haskell code: `putStrLn "Hello, Haskell!"` and GHCi immediately
evaluate and execute it.

After that, we type the `:q` command to leave GHCi.


Here's an example of using `ghc` to compile a Haskell source file
and then running the compiled executable:

```
suppi@ubuntu:~$ cat > Main.hs
module Main where

main :: IO ()
main = putStrLn "Hello from Haskell!"
^C
suppi@ubuntu:~$ stack exec ghc Main.hs -- -o HiHaskell
Run from outside a project, using implicit global project config
Using resolver: lts-6.14 from implicit global project's config file: 
/home/suppi/.stack/global-project/stack.yaml
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking HiHaskell ...
suppi@ubuntu:~$ ./HiHaskell 
Hello from Haskell!
```

We can also just run the file without compiling it using `runghc`:

```
suppi@ubuntu:~$ stack exec runghc Main.hs
Run from outside a project, using implicit global project config
Using resolver: lts-6.14 from implicit global project's config file: 
/home/suppi/.stack/global-project/stack.yaml
Hello from Haskell!
```

By default, Stack will only use the standard Haskell library called `base`.
If we'd like to use external packages, for example: `http-client`, we can use
the `--package` flag. This flag will download the package specified and its dependencies
the first time, so it may take some time to load, but the next time you will try it,
Stack will use the packages it downloaded the first time.

Here's a short example of running a `GET` request using http-client from GHCi:

```
suppi@ubuntu:~$ stack exec ghci --package http-client
Run from outside a project, using implicit global project config
Using resolver: lts-6.14 from implicit global project's config file: 
/home/suppi/.stack/global-project/stack.yaml
base64-bytestring-1.0.0.1: download
base64-bytestring-1.0.0.1: configure
base64-bytestring-1.0.0.1: build
base64-bytestring-1.0.0.1: copy/register
data-default-class-0.0.1: download
data-default-class-0.0.1: configure
data-default-class-0.0.1: build
data-default-class-0.0.1: copy/register
mtl-2.2.1: download
mtl-2.2.1: configure
mtl-2.2.1: build
mtl-2.2.1: copy/register
network-2.6.3.1: download
network-2.6.3.1: configure
network-2.6.3.1: build
network-2.6.3.1: copy/register
old-locale-1.0.0.7: download
old-locale-1.0.0.7: configure
old-locale-1.0.0.7: build
old-locale-1.0.0.7: copy/register
random-1.1: download
random-1.1: configure
random-1.1: build
random-1.1: copy/register
stm-2.4.4.1: download
stm-2.4.4.1: configure
stm-2.4.4.1: build
stm-2.4.4.1: copy/register
async-2.1.0: download
async-2.1.0: configure
async-2.1.0: build
async-2.1.0: copy/register
tagged-0.8.4: download
tagged-0.8.4: configure
tagged-0.8.4: build
tagged-0.8.4: copy/register
text-1.2.2.1: download
text-1.2.2.1: configure
text-1.2.2.1: build
text-1.2.2.1: copy/register
blaze-builder-0.4.0.2: download
blaze-builder-0.4.0.2: configure
blaze-builder-0.4.0.2: build
blaze-builder-0.4.0.2: copy/register
cookie-0.4.2.1: download
cookie-0.4.2.1: configure
cookie-0.4.2.1: build
cookie-0.4.2.1: copy/register
hashable-1.2.4.0: download
hashable-1.2.4.0: configure
hashable-1.2.4.0: build
hashable-1.2.4.0: copy/register
mime-types-0.1.0.7: download
mime-types-0.1.0.7: configure
mime-types-0.1.0.7: build
mime-types-0.1.0.7: copy/register
parsec-3.1.11: download
parsec-3.1.11: configure
parsec-3.1.11: build
parsec-3.1.11: copy/register
network-uri-2.6.1.0: download
network-uri-2.6.1.0: configure
network-uri-2.6.1.0: build
network-uri-2.6.1.0: copy/register
transformers-compat-0.4.0.4: download
transformers-compat-0.4.0.4: configure
transformers-compat-0.4.0.4: build
transformers-compat-0.4.0.4: copy/register
exceptions-0.8.3: download
exceptions-0.8.3: configure
exceptions-0.8.3: build
exceptions-0.8.3: copy/register
unordered-containers-0.2.7.1: download
unordered-containers-0.2.7.1: configure
unordered-containers-0.2.7.1: build
unordered-containers-0.2.7.1: copy/register
semigroups-0.18.1: download
semigroups-0.18.1: configure
semigroups-0.18.1: build
semigroups-0.18.1: copy/register
case-insensitive-1.2.0.7: download
case-insensitive-1.2.0.7: configure
case-insensitive-1.2.0.7: build
case-insensitive-1.2.0.7: copy/register
http-types-0.9.1: download
http-types-0.9.1: configure
http-types-0.9.1: build
http-types-0.9.1: copy/register
zlib-0.6.1.1: download
zlib-0.6.1.1: configure
zlib-0.6.1.1: build
zlib-0.6.1.1: copy/register
streaming-commons-0.1.15.5: download
streaming-commons-0.1.15.5: configure
streaming-commons-0.1.15.5: build
streaming-commons-0.1.15.5: copy/register
http-client-0.4.31: download
http-client-0.4.31: configure
http-client-0.4.31: build
http-client-0.4.31: copy/register
Completed 25 action(s).
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> import Network.HTTP.Client
Prelude Network.HTTP.Client> manager <- newManager defaultManagerSettings
Prelude Network.HTTP.Client> request <- parseRequest "http://httpbin.org/get"
Prelude Network.HTTP.Client> response <- httpLbs request manager
Prelude Network.HTTP.Client> putStrLn (read (show (responseBody response)))
{
  "args": {}, 
  "headers": {
    "Accept-Encoding": "gzip", 
    "Host": "httpbin.org"
  }, 
  "origin": "5.102.241.151", 
  "url": "http://httpbin.org/get"
}
```

### What's Next

That's it for this guide. There is of course more to learn, but this
should be enough to start learning from different Haskell learning materials
using Stack.

What's next?

1. [Pickup a learning resource](https://haskell-lang.org/documentation). I can recommend [Haskell Programming from First Principles](http://haskellbook.com), but there are more resources available.

2. [Read the Stack documentation](https://haskellstack.org) to learn more about building and managing Haskell programs and libraries using Stack.

3. [Another, more thorough guide to getting started using Stack](http://howistart.org/posts/haskell/1)

4. [How to write great documentation for Haskell code](http://yannesposito.com/Scratch/en/blog/Haskell-Tutorials--a-tutorial/)

5. [An example for a non-trivial Haskell project](https://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-1-trees.html)

6. [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/) - A really long reference for a lot Haskell related concepts, features and libraries. Also links to other materials by subject


Good luck learning Haskell! I hope you'll enjoy it!
