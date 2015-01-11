# Realization of a shitty project, using awesome tech (Part Idontknow-IV)

Here we are again! A lot has happened behind the scenes since the last time, yet so little has changed from a user perspective.

## Under attack!

First of all, after just a few weeks of uptime, one night I woke up in the middle of the night thinking "OMFG! The [EPMD](http://www.erlang.org/doc/man/epmd.html) port is not secured, and I'm using the default erlang cookie!".

Well not exactly a big deal, I have no sensible data to protect and the server runs as an unprivileged user anyway. The following morning, first thing I logged into my server and thrown a bunch of commands on the shell. Nobody tried to join the node via EPMD, however netstat and ps were showing a couple of ative ssh connections that definetly weren't me. From chinese IPs.

Long story short, since I don't know how to do a proper assessment of violated resources I grabbed the last db backup, revoked certificates, disintegrated the VM and recreated it from scratch (with some more hardening, this time).

How did they get in? Turns out that a year ago, when I bought the VM and configured it I totally forgot to disable password login. And looking at `/var/log/auth.log`, no doubt that it was a stupid bruteforce attack. Fun thing is that it was the default password assigned from the hosting service provider that even I totally forgot.

## Improving automation

So behind the scenes I worked on the automation. I replaced [rebar](https://github.com/rebar/rebar) with [erlang.mk](https://github.com/ninenines/erlang.mk), a choice that came mostly from a feeling that I got reading around teh interwebz but that reavelead itself to be quite good. It doesn't support multi-app projects (the `./apps/` dir of rebar, so to speak), I think that helps to apply the concept of use before reuse. Create a reusable component along the rest of your application, spike it, evolve it until it stabilizes and has a shape good enough to be really extracted in its own single-app library project.

I'm still a noob at release handling, but the defaults with [relx](https://github.com/erlware/relx) as a release tool makes for way smaller node tarballs, that I appreciate as my upload bandwith is ridiculous. Halving the dimension halved really helps in having faster deploys.

And a last thing, I'm a fan of type checking and dialyzer, but it's insanely slow and having to wait minutes at every compilation is daunting. In erlang.mk type checking tasks and test tasks are not in the default target, so I get faster compilations and give a meaning to [travis CI](travis-ci.org/caligin/netronner) to run these for me. Therefore I also started to write some tests (yeah, I know, I should have done it from the beginning). I wanted to start with small unit tests, so I chose [EUnit](http://erlang.org/doc/apps/eunit/chapter.html) over [Common Test](http://erlang.org/doc/man/common_test.html). But erlang.mk supoprt for eunit was still being developed, so I had the chance to give my little contribution to it on github. 

I also intorduced [grunt](http://gruntjs.com/) and [bower](http://bower.io/) as automation tools for the UI project, in order to have automatic dependency management. I'm unsure of the result: I traded 25 lines of a simple Makefile with 60+ lines of [Gruntfile](http://gruntjs.com/sample-gruntfile) plus all the [nodejs](http://nodejs.org/) and bower configuration. It adds a little complexity I think, but I'll be able to introduce plugins easily later on (like minification, that I'm not doing at the moment). And the dependencies are no longer committed into the repository. The transition was rather smooth as the available plugins allowed me to do everything I needed. The exception was the check for uncommitted changes in the git repo, but the grunt API allowed me to write that task quite easily. I'm pretty statisfied with it, in the end.

## Changing design

Small happened on the actual application code. I worked on the achievements to introduce implications and series. The reasoning is that's impractical to award manually all the ranks of a ranked achievement when you unlock, to say, rank four. But since some achievements behave *almost* like this but in a nonlinear fashion, I decided that an implication mechanism would be more effective. The series information is used on the UI to show only the highest rank unlocked of a series. It's rather pointless to show the previous steps, isn't it?

So while modifying the achievemnts I also thought: "Hey but why did I separate UI and API and the display informations are still in the API?". I'm talking about the icon field. I decided that it's an information not pertinent to the API and moved it to the UI, where it actually makes sense to be.

I also changed the achievements definitions to be a `.erl` module instead of mutable application state. Being able to change them at runtime, or configure them in general is quite useless as NetRonner is not really a reusable and customizable application right now but rather a private system of Tiles of Fury. If it ever evolves to the point when it might be useful to mahjong clubs around the world I'll consider to reintroduce the possibility of customize this aspect, but in the meanwhile I'm going to commit the achievements in the repository and treat that piece of data as actual code.

## What's next

With the stabilization of this first release, the next big thing at the horizon is keeping track of game scores and ultimately a leaderboard.

I was about to rush into immplementing the ranking system, but it makes no sense if we can't upload game data yet. And being able to keep game data has no meaning unless there's a tool to do so. The result is that I started working on the [Android app](https://github.com/caligin/netronner-android) for NetRonner. Why? Because when we play we have our phones by hand, but not the laptops. So it's natural to use that, and not the web UI for this task.

Finding a good design won't be easy, though. There are a lot of stats I'd like to keep track of in order to let players self-evaluate their ability, but this is not a videogame so someone has to do the boring data insertion job. Balancing completeness of information and simplicity of use will be a good challenge.

However even if I put aside the ranking system for now, I still looked into it a little and the only matchmaking algorithm I found that supports games with more than two players/teams and is not based on how much points you score is Microsoft's [TrueSkill](http://research.microsoft.com/en-us/projects/trueskill/) system. So I guess I'll go for reimplementing it in Erlang when the time comes.

Wow, few things but a lot to write about! That's it for this time, see ya next!