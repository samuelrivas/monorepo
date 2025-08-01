## Goal

This is a simple password manager written in Go, with the main goal of writing
something in Go for learning purposes. That said, it's useful — I use it to
manage all my passwords.

## Principles

- Minimise complexity. I’m happy to accept some awkwardness if the alternative
  means adding complexity. It’s a balance, but I’m more complexity-averse and
  less awkwardness-averse than your average random tool writer. This shows up in
  a few ways:
  - I use S-expressions instead of JSON. This isn’t the cleanest decision in Go,
    since there’s no good, simple, standard S-expression parser. So I shell out
    to [Jane Street’s `sexp` tool](https://github.com/janestreet/sexp) to manage
    the password file. Maybe that’s not *technically* simpler, but I made the
    call, so for now: yes :P
  - I don’t use command-line flags. Parsing is easier if I just stick to
    positional arguments. No library needed, and it works. Yes, a bit awkward,
    but if you’ve read this far, you get the deal.
  - I don’t use GPG. I’m not a cryptographer, but I can tell GPG is
    complicated. There's plenty of debate about whether it’s even safe to use. I
    only need symmetric encryption — authenticated is nice but not essential
    (the file lives on my laptop; if that’s compromised, I have bigger
    problems). [Age](https://github.com/FiloSottile/age) supports authenticated
    symmetric encryption, has a good CLI (critical), and a Go library (saves me
    from more shelling out :P). There’s some concern about Age’s authenticated
    encryption story for public key use, but symmetric with
    [Scrypt](https://en.wikipedia.org/wiki/Scrypt) looks solid.

- Implement a CLI that works via stdin/stdout and avoids obvious footguns like
  leaking passwords in CLI arguments.

## How to use it

Passwords live in an encrypted file containing a S-expression: a list of
“objects”. Each object is a list of pairs, where the first element is the
key. Example:

```
((site my.site) (password hunter22))
```

Since this is a cheap tool, you need to bootstrap the file — it won’t work with
an empty one. Something like this will do:

```
echo '(((site my.site) (meta initial-site)))' | age -p > passman.age
```

You can later edit the `meta` value, add a `password`, or more fields. Add as
many sites as you want.

If you break your password file, decrypt with `age -d`, fix it, then re-encrypt.

If you’re still reading, just run `passman --help` and hope for the best ;-)
