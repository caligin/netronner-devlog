# Realization of a shitty project, using awesome tech (Part VII)

Hello again! Once again an update wthout any actual improvement on the project! But I have something to say nonetheless.

Last thing first: I moved to London with only [Yuki](http://fav.me/d4fmozc) (my netbook). She's a bit old and too low-spec to handle [Android Studio](http://developer.android.com/tools/studio/index.html) so since I was focusing on the Android app I'm kinda stuck. I might decide to prioritize the new features in the backend even if I think it's kinda wrong to implement those until there's the data source to feed them.

## Relxase it!

A front where I tried to do better is the relase process. My goal is a one-click-deployable application. Usually the process is handles with some automation tool like [Puppet](http://puppetlabs.com/) but before introducing such a tool I wanted the ability to create an artifact that requires the least possible other automation to be deployed. That's because the config management part is not general-purpose but specific of my installation, therefore I don't want it to show up on public github, where I want the most possible of the platform.

 Right now the release process is the following:
- commit, push new changes
- wait for [Travis CI](travis-ci.org/caligin/netronner) to run all tests
- tag release
- `make` a containerized build of the erlang release tarball
- scp tarball to server
- ssh to server
- on server there's a [Makefile](https://www.gnu.org/software/make/manual/html_node/Makefiles.html) to stop the node, extract the tarball in the correct location, set permissions and restart the node
- when configuration changes occurr I also have to edit the [sys.config](http://www.erlang.org/doc/man/config.html) file
- upgrade all the version numbers to the new version

Plus, the generated node startup script is broken when stopping the node and I forget everytime. What happens it that doesn't really stop the node so I end with the old node still running and the new one that keeps crashing because it can't open the same ports twice. And [heart](http://erlang.org/doc/man/heart.html) keeps restarting it. So stopping both always ends up being a good amount of pain.

One strong points of Erlang is that it features the capability of doing hot-code upgrades of a whole release ([relup](http://www.erlang.org/doc/man/relup.html)) or just an application ([appup](http://www.erlang.org/doc/man/appup.html)). However the use of these feature is not recommended unless you really have a strong SLA requirement. Why? Because it becomes important to do intensive testing of the upgrade scripts. Also I have absolutely no SLA requirement, even if I'd like to at least avoid dropping a connection that's in the middle of a write operation. It's one of those things that gives users a bad experience with the interface.

So I won't exactly use appups and relups, but since they exist I figured out there should be some kind of facility to generate at least an upgrade package to drop on the server and just restart it with the new code. The release tool used by [erlang.mk](https://github.com/ninenines/erlang.mk) is [relx](https://github.com/erlware/relx), so I tried to dig deeper into it. I found out that it handles creating a relup tarball but it needs both the old and new versions of the required applications, a thing that would force me to write some weird scripts to figure out the deployed version, check out it, build it, do the same with the latest and then generate the relup. A failure, I must say. But at least I had a good reason to read how appups and relups work, and know a little better what an erlang release actually is.

Anyway I really want an automation tool for this task. Possibly not Puppet or, more precisely, possibly without the need for a server component to handle the automation. I've always used Puppet in masterless mode with a bunch of custom scripts and I have to say, it's not really a tool made to release software by itself (masterless, at least) but more something to do the first deploy and then keep configurations in check. As far as I know, to have something more to handle upgrades and migrations you need to combine it with [MCollective](https://puppetlabs.com/mcollective) which I know nothing about.

So I was about to try [Ansible](http://www.ansible.com/home), that's *almost* agentless (still requires python on the target machine) but I'm about to change the server infrastructure to another provider so before throwing myself in automating something that's about to change I'll check on the features of the new provider first. It should support [Docker](https://www.docker.com/) but it's not where I'm willing to go. I'm still wary of Docker use in production for security concerns and I think it's way less overhead to just move around the bare release tarball and assign a handful of permissions (Plus, Yuki's a 32-bit so can't run Docker on her to develop).

## Kill la Switch

So thinking about the possibilities of the new infrastructure I had in mind the possibility to have an [ImmutableServer](http://martinfowler.com/bliki/ImmutableServer.html). An upgrade would mean bring up another machine, provision it with the new Netronner version, DNS-swap the instances then kill the old one. But I have to copy the database in the process and that may lead to a lost-update problem if I am to allow write operations on the API while copying. To avoid the problem while leaving read-only access active in the process, I implemented a killswitch to deny write operations. When set, the API will respond with a 503 retry-after to any write operation until I'm done migrating.

This hassle because I belive that for the UX a "sorry you're not allowed some operations for a while" is better than a "whole site is down LOL!". It's not a real requirement as the users are still just 20 and they're not constantly on the site for sure, but since Netronner is also a study in design/architecture I'll pretend it actually matters.

## Micro parenthesis on micro services

I didn't dug deeper in the microservices yet, but the common definition is "focused components individually manageable that speak a common interface". What pops in mind after understanding what Erlang applications and releases are is that the concepts of microservices are already baked in the platform. Applications are individually manageable, deployable, upgradable. They are a cohesive collection of elements concurring to build a focused functionality and they all speak the common language of Erlang interprocess messaging. Of course with this you don't get the nice thing about polyglot services but you obtain standard tools and battle-tested practices and libraries to handle them.

## These are not the Androids you're looking for

Like anticipated in the last issue, I started developing the Android app for Netronner.

At the end of our Mahjong games we usually write a rapid note with the end score of the game and then post it on our g+ community. However the notes are sometimes a bit messy and not very precise, plus not everyone has a good confidence with hand scoring and the point tables. And we have the long-term goal of a leaderboard plus some statistics to be displayed on Netronner, to keep record of our gameplay (something like games played, wins, losses, tsumos, rons, deal-ins...).

The basic thing that the app should be able to be is to store some data about a game.

This will naturally lead to the possibility to upload and display those data on the server, then to the statistics and ultimately the leaderboard.

To start an iterative process the very basic thing is to be able to create a list of games with some basic data to be further enhanced later with the per-hand information. I started sketching an interface with some built-in mock data. The first prototype was something like the g+ app, with the games represented like big rectangular items with lots of information: date of the game, the four players with their g+ profile picture, name and initial wind seating and an indication of the game being an [Hanchan][1] or a [Tonpuusen][2]. It ended up being cluttered and uncomfortable to browse, so I dropped everything that wasn't the minimal necessary information to identify a game from the main list. The game type wasn't an interesting information at all and was dropped, the player positions will be useful later but not for display on the list, the order of the profile pictures is enough to represent the positions. The new layout is much more compact, requires less components to be built and is more fit to be displayed in a [ListView](http://developer.android.com/guide/topics/ui/layout/listview.html).

While designing this small piece, I clashed with the appcompat libraries. I've picked up Android after several months since last time, and last time I was wandering around a bit randomly anyway. Android Studio, by default, tries to drop in the appcompats anytime it can. However finding and learning the correct components, views and layout parameters is demanding by itself even without the parameters that should be replaced with the appcompat ones in order to be backwards compatible. To start I'll target my device only (that's old enough, little Hibiki has Android 4.1.2), so I dropped the appcompats entirely.

## Curse the cursors

Next is the part when I started doubting Android. First time I approached it, a year ago, I did so with [Coursera](https://www.coursera.org/)'s courses held by professors Porter, Schmidt and White. The impression it gave me is that the Android programming framework was beautifully built on well understood, widely known design patterns and principles. Even if I didn't like very much the extend-by-inheritance model. And I enjoyed the [developer guides](https://developer.android.com/guide/index.html), concise yet clear about the meaning of the various components.

Now, according to those guides (and my interpretation):
- To store structured data that might be queried, the storage of choice is SQLite.
- It is possible to implement a [ContentProvider](http://developer.android.com/guide/topics/providers/content-providers.html) upon it, but that should be if you want to expose the data to other apps.
- That data should be rendered in a ListView via an adapter.
- It should be loaded asynchronously via a [Loader](http://developer.android.com/guide/components/loaders.html).

Given that, plus my assumption that having a handful of querable data that I want to query but don't want to expose is the most common thing I might want out of my application, I expected the basic components to implement such a simple case were ready for me to use. And in fact I found CursorLoader... that expects to be constructed with an URI. URIs are used to query data from ContentProviders, that I don't want. The only other alternative is implementing it as an AsyncTaskLoader. Fine, after all extending AsyncTaskLoader only requires to override `T loadInBackground()`, where I should issue the query and return the cursor. Easy. Nope. Turns out that the cursor never loads. The deafault implementations for several other methods of AsyncTaskLoader are noops, that is useful by itself in no situation. You'll always have to either force a load everytime (overriding `void onStartLoading()`) or do the complex but correct thing of loading only what you need (overriding almost everything) and implementing some kind of Observer on the data source to invalidate the local cache.

Levels of abstraction are supposed to shield users from the complexity of the underlying implementations, however in this case having the methods default to noops and no warning about this on the developer guides it ends up forcing some complexity of the loader model on a developer that is totally unaware of the problem. Plus, there's no default facility to observe data insertions in a SQLite database. Guess I'll use a [BroadcastReceiver](http://developer.android.com/reference/android/content/BroadcastReceiver.html).

## What's next

Thare's no real "next" as what I tried is still halfway. I'll try to complete the infrastructure migration first, and then I'll see.

See ya next!


[1]: #"A standard Riichi Mahjong game, consisting of an East round and a South round. See http://osamuko.com/big-list-of-japanese-mahjong-terminology/ ."
[2]: #"A Riichi Mahjong game consisting of the East round only. See http://osamuko.com/big-list-of-japanese-mahjong-terminology/ ."