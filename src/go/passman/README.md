## Goal

This is a simple password manager written in go, with the main goal of writing
something in go for learning purposes. It is nonetheless useful, I use it to
manage all my passwords.

## Principles

  - Minimise complexity. I accept some awkwardness if the solution requires
    complicating things. It is obviously a balance, but I am more complexity
    averse and less awkwardness averse than your average random tool
    writer. This manifests in this project in several ways:
    - I use S-expressions in favour of json. This is not necessarily a clean
      decision in this project, as go doesn't seem to have a good, simple,
      standard s-expression parser, so it carries with it a little hack that I
      use to manage the password file: I shell out to [Jane Street's sexp
      tool](https://github.com/janestreet/sexp). We can argue whether this
      serves the simplicity goal, but since I made the call, the answer is yes,
      for now :P
    - I don't use command line flags. Command line parser is a lot easier if I
      just depend on positional arguments, which doesn't require any cli parsing
      library at all, and works. It is a bit awkward, yes, but if you read this
      far, you know what's the deel.
    - Don't use GPG. I'm not a cryptographer, or a cryptography engineer, but I
      can see that, at the very least GPG is very complicated. There is a fairly
      hot debate online about whether it is safe to use or not as well. I only
      want symmetric encryption, and I appreciate if it is authenticated, but is
      not necessary (all in all, the encrypted file is in my laptop, if someone
      poisons it I probably have bigger problems to worry
      about). [Age](https://github.com/FiloSottile/age) seems to do
      authenticated, symmetric encryption fairly well, and comes with a
      convenient cli, which is a must, and a go library (which saves me from
      shelling out again :P). I found some concerns about the authenticated
      encryption story for public key cryptograpy with Age, but symmetric
      encryption with [Scrypt](https://en.wikipedia.org/wiki/Scrypt) seems to be
      fairly solid.
  - Implement a cli that works with standard input and ouptut, and does a fair
    job at preventing passwords to leak (e.g. don't ask the user to pass the
    password as a cli argument).

## How to use it

The passwords are stored in an encrypted text file containing a S-expression,
that is a list of "objects". Each object is a list of pairs, where the first
element of the pair is the record name. So for example, an object would look
like this:

```lisp
((site my.site) (password hunter22))
```

Since this is a cheap tool, you need to bootstrap the file before using it, it
won't work with an empty file. This does the trick nicely:

```sh
$ echo '(((site my.site) (meta initial-site)))' | age -p > passman.age
```

You can later edit the `meta` value if you want or add more fields to `my.site`,
including a `password` field. You can, of course, add more sites.

If you ever break your password file, decrypt it with `age -d` and then encrypt
it again.

In the unlikely event that you're still reading, just rune `passman --help` and
hope for the best ;-)
