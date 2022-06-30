_Note, 2022-06-011_: this repository is alpha-level software. Stability or correctness should not be assumed.

# `plutarch-context-builder`
Public discussion and assistance can be found on [discord](https://discord.gg/yGkjxrYueB) @ #liqwid-plutarch-extra

## What is this?

A library containing builders for `ScriptContext`s. Currently, we support
building `ScriptContext`s with the `Spending` and `Minting` `ScriptPurpose`s.

## What _exactly_ does this do for me?

Building a `ScriptContext` manually is sometimes necessary: testing is a good
example case. However, doing this directly is tedious, error-prone and extremely
unclear in many places; this leads to significant wasted time and confusion. 

`plutarch-context-builder` aims to make this as easy as possible. We do this
through a combination of higher-level operations to describe what the script
should do, along with a `Semigroup`-based API to put things together with. If
you use this library, you don't have to deal with the particulars of how
`ScriptContext`s are defined if you don't want to, but you _can_ still generate
them reliably and with minimal pain.

# How do I use this?

Use either the `Spending` or `Minting` module's API to construct a builder; you
can combine builders with `<>`. Then, produce the required `ScriptContext` by
providing a `ContextConfig` from the `Config` module.

# What can I do with this?
