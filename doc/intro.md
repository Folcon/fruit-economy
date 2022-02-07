# Introduction to fruit-economy

TODO: write [great documentation](http://jacobian.org/writing/what-to-write/)

# Next Steps
There's clearly a bunch of stuff that needs to be done first before I can pull the changes started at `7a72fb5` into the system.

For a start the current sim data structure is beginning to get unwieldy, which means some approach for either navigating it or denormalising it.
Which means reaching for a tools like, [`specter`](https://github.com/redplanetlabs/specter), [`pathom`](https://github.com/wilkerlucio/pathom) or [`datascript`](https://github.com/tonsky/datascript/).
In this case I'm tempted to just reach for denormalisation with `datascript`, it still gives me a lot of future options would allow me to use [`sqlite`](https://github.com/someteam/acha/blob/0.2.0/src-clj/acha/db.clj#L67) or [`datomic-local`](https://docs.datomic.com/cloud/dev-local.html) as a local store.

More importantly `datascript` isn't too strict about denormalisation, so if I want to keep pieces nested I can. I'm hoping that by denormalising the more complex bits I'll simplify enough of my state that I can start seeing nice opportunities to refactor.

However, I also don't want to just jump in and do that, that way leads to mess, let's figure out how to integrate the changes into the main sim, so we can see in the game ticks leaders making decisions and subordinates following them on a per-civ basis. Then we can look at how to refactor all of this.

Post that denormalisation refactor would be a good opportunity to start writing some integration tests to cover game functionality. Alongside that we'll probably want to think about where we want to draw system boundaries.
