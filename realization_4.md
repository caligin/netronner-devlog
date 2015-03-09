# Realization of a shitty project, using awesome tech (Part IV)

Here I am, another update, nothing has been done (almost nothing, we actually started [some experiments](https://github.com/SHWB/pytile) with the [OpenCV libraries](http://opencv.org/) to implement the hand scoring service/Android app).

Lately I spend too much time thinking an end up doing everything backwards. So this time, while looking at my "deployment requirement list" I found out that among the docker hosting providers I was looking at, [DotCloud](https://www.dotcloud.com) bills by allocated memory at 32MB blocks. And today [this tweet](https://twitter.com/erlangbikeshed/status/532477528121040896) happened to be in my feed (just to be clear, I do not think it's shitty advice). Not that matters, but I felt that if we count this, how the app should be divided in containers and volumes (still no clue), how is this billed and all the complexity derived from [Riak](http://basho.com/riak/), well... I'll just drop Riak.

I played with it a bit and have a rough idea of its model (very rough). But I spent more focus on it than on other things that could be more important (build automatization, ssl...). But I still didn't learn decently how to manage it (and in the middle of this TODO mess I don't want to) and super-fast or memory efficient it may be, if the app has ten freaking users and no one more, it's just overhead (no mesuraments needed). No, really, I have no SLA requirement, no need for replication, scaling or other buzzwordy stuff.

And that's it. I might also think of dropping [Docker](https://docker.com/) (/cry). I don't want to, but I actually have a server to deploy netronner and I'll be an idiot not using it just because there's no Docker on it.

Well that's it. I might find some more simplifications to do before going to prod, don't know yet.
See ya again!