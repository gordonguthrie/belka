# Belka 2️⃣🐕🚀

A [Gemini Server](https://gemini.circumlunar.space/) in Erlang named after Belka - the second dog in space

# Architecture Documents

The [architectural documentation](https://gordonguthrie.github.io/belka) for this example is built with [Literate Code Reader](https://gordonguthrie.github.io/literatecodereader)


# Installation Guide

To use Belka please see [Belka Example](https://github.com/gordonguthrie/belka-example)

# Design Principles

The Belka server family is designed to be as smol as possible, so features are only to be dragged in as needed.

To that mind various services are provided as additional components:

* [Belka Router](https://github.com/gordonguthrie/belka-router) - a URL router
* [Belka Templates](https://github.com/gordonguthrie/belka-templates) - simple text-based templating system

# A Worked Example

A fully featured example system using both URL routing and templating can be found at [Vega & Altair](https://github.com/gordonguthrie/vega_and_altair)