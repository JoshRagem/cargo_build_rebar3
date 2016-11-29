rustbar
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rustbar, ".*", {git, "git@host:user/rustbar.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rustbar
    ===> Fetching rustbar
    ===> Compiling rustbar
    <Plugin Output>
