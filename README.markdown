comonad
=======

[![Build Status](https://secure.travis-ci.org/ekmett/comonad.png?branch=master)](http://travis-ci.org/ekmett/comonad)

There are two ways to define a comonad:

I. Provide definitions for 'extract' and 'extend' satisfying these laws:

    extend extract      = id
    extract . extend f  = f
    extend f . extend g = extend (f . extend g)

In this case, you may simply set 'fmap' = 'liftW'.

These laws are directly analogous to the laws for monads
and perhaps can be made clearer by viewing them as laws stating
that Cokleisli composition must be associative, and has extract for
a unit:

    f =>= extract   = f
    extract =>= f   = f
    (f =>= g) =>= h = f =>= (g =>= h)

II. Alternately, you may choose to provide definitions for 'fmap',
'extract', and 'duplicate' satisfying these laws:

    extract . duplicate      = id
    fmap extract . duplicate = id
    duplicate . duplicate    = fmap duplicate . duplicate

In this case you may not rely on the ability to define 'fmap' in
terms of 'liftW'.

You may of course, choose to define both 'duplicate' /and/ 'extend'.
In that case you must also satisfy these laws:

    extend f  = fmap f . duplicate
    duplicate = extend id
    fmap f    = extend (f . extract)

These are the default definitions of 'extend' and'duplicate' and
the definition of 'liftW' respectively.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
