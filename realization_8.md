# Realization of a shitty project, using awesome tech (Part VIII)

Ohai!

Not really much to say since last time, except that *the project is stalled*.

## Stalled project is stalled

Unfortunately since I moved to London Tile Of Fury's Mahjong games stopped, so the app lost, like, 100% of its value.
Plus, in the meantime I became aware of so many new facets of programming, UI design, Ops stuff etc. etc. that keeping the project up was just unfeasible.

I learnt great insights that I want to apply to it, but the time to do so is little, the number of changes is great and the IAAS provider I was using just pissed me off for a number of reasons.

So for now Netronner will wait in my "TODO list" until I find a suitable moment to use it again as a playground of what I learnt in my journey.

As soon as I'm able to work on it again, here's the list of things that I'd like to do on it:

- de-prioritize the Android client
- destroy the UI and rebuild it as a mobile-first, progressive-enhanced, no-js support, semantic UI.
- get a proper [Continuous Delivery](https://www.thoughtworks.com/continuous-delivery) pipeline.
- apply security-aware checks on the UI (avoid XSS, session hijacking and friends)
- throttling &| rate-limiting &| circuit-breakers on downstream calls (only google-auth for now)
- proper & semantic monitoring
- secure properly the API properly configured firewalls, rate-limiters & friends
- maybe a redesign to support private, customized deployments for various Mahjong clubs

## Let it go

Sometimes something is just not feasible. Previously I said that I wanted to integrate Microsoft's [TrueSkill](http://research.microsoft.com/en-us/projects/trueskill/) algo for the leaderboards, but apparently it's patented. That means it's a no-go and, unfortunately, I have to drop it. I'll figure an alternative solution when the time comes (Maybe implementing a restricted subset of it saves me from the legal madness? IDK, but if you have insights please reach to me).

## What's next

Well, as I said, nothing for now. I'll pick up the project again in the future for sure, but as now that's what happening and nothing new will float around for a while. You can check out [my twitter](http://bit.ly/1LvBoGU) for new updates.

Seeya!