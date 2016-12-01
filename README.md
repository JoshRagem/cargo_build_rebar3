rs_compile
=====

A rebar plugin to `cargo build` rust source files

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rs_compile, ".*", {git, "git@github.com:JoshRagem/rs_compile.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rs compile
    ===> Fetching rs_compile
    ===> Compiling rs_compile
    <Plugin Output>
