# Realization of a shitty project, using awesome tech (Part V)

We're in production, finally! The site is available at [ron.gg](https://ron.gg).

While dropping [Riak](http://basho.com/riak/) I foud the [Erlang programming rules and conventions](http://www.erlang.se/doc/programming_rules.shtml), and I started doubting the stucture I gave to the application.

The structure up until now was directed by the concept that we use in Java, called "package by feature". Basically, stuff that fit together (and, more importantly, change at the same speed) fit in the same package. This way package dependencies become feaure dependencies as stuff that belong to the same feature naturally ends up in the same package. Trying to translate that in Erlang, Netronner is made of serveral Erlang applications, one for each feature (achievements, aaa, players, google, timeline...) plus one that depends on everyone else, contains the actual HTTP api and serves as the main entrypoint (netronner module itself). Every other app (I call them "satellite" apps) has a [gen_server](http://www.erlang.org/doc/man/gen_server.html), an [application](http://www.erlang.org/doc/man/application.html) and a [supervisor](http://www.erlang.org/doc/man/supervisor.html).
But isn't that a lot of overhead? I mean, why do I have to have all this boilerplate for no gain? Also, while a [Cowboy](https://github.com/ninenines/cowboy/) handler spawns a process per request, a gen_server serializes requests. Isn't it a bottleneck?

I was also concerned by the configuration mechanisms (there are a few variables that have to be configured on a per-instance basis, like the googe api access tokens, database files locations and such). I'm used to a [Spring](http://projects.spring.io/spring-framework/)-like approach, where a single point (the application context) instances and passes configuration to all the components in the application. So having the satellite applications load their own variables via [application:get_env](http://www.erlang.org/doc/apps/kernel/application.html#get_env-1) sounded weird.

Long story short, what I did was refactoring everything to be a library app. I also took the chance to change the all-in-one json handler to several [Cowboy REST handlers](http://ninenines.eu/docs/en/cowboy/HEAD/guide/rest_handlers/) (more on that later). So the result is that the nertronner application (and supervisor) callback modules also have the role of a Spring application context of loading parameters, building all the required resources, opening handles to [dets](http://www.erlang.org/doc/man/dets.html) tables and pass the references to these resources to the [REST](http://en.wikipedia.org/wiki/Representational_state_transfer) handlers that depends on them. This felt better than before, aside the google tokens env variables that had to be passed around for a couple of level of indirections (due to laziness, in the end they still come from separate app envs instead of being centralized on the netronner module).

That is, until I had to implement the award_achievement handler. Because when you want to award an achievement to a player, the following steps occur:
- Lookup for the player. It must either exist or be a valid g+ user.
- If it doesn't exist, get the g+ user data and use it to inizialize the new player
- Either case, load the player data and the achievement data (that must exist)
- Add the achievement data to the player data (in memory)
- Overwrite the persisted player data with the enhanced version
Now, when the players feature (the one with the award_achievement function) is a library app, which functions are called by possibly-concurrent instances of a handler, does it look like a den of possible race conditions? Yes it does.

So with that and [this, page 40](http://ninenines.eu/talks/oscon2012/oscon2012.html) (that I found in the meantime) in mind, I realized why it's sane to have a gen_server in your features: you can decide the access pattern to your resouces. It's serialized by default, but you can change it to concurrent in a few lines if you want. Doesn't matter which it is, you can choose, and encapsulate your policies behind the api callbacks of your gen_server module.

Deploying also made me doubt the choice of having a single repository for api, ui and the devlog: what if I want to release a new version of api? I'll of course tag it on git, but the ui gets tagged with the same version at the same moment. While that's not a big problem in this direction, I find it annoying in the opposite one: I expect the api to be way more stable than the UI. Tagging multiple times, with different versions an api that never changes feels wrong. Moreover, I'll soon start the Android app component and I expect it to have its own independent versioning. So I split the git repo in three independent parts.

For the same reasons, I thik I should drop the satellite apps as they are and make them modules in the main app. Updating the version means updating every version: I made a fix in the players app and bumped it from 1 to 1.0.0 (yeah, I accidentally made the first release whit a one-digit versioning pattern). Should I consequently bump netronner (that depends on players) ? I don't think so as I changed an internal and that required no intervention on dependant modules, but then which is the actual version number for the whole release? I feel that the applicative versions should be aligned with the release version. And I also feel that, even if the satellite apps can exist without the whole, they make no sense without it. That to say, I think I'll pull eveything in a single cohesive application (except the google api integration module, that might become an actual independent app one day).

What else? Oh, yeah, about REST: I read some more on the topic and understood that I actually know nothing about it. "Verbs in URIs are bad" and stuff like that does not matter. And I learnt that you can't to "real" REST if you use a media type that does not support linking (like JSON, which I'm using). So first thing I stopped calling it a REST api and started calling it a REST-like api, for the sake of correctness. And second thing I bought the [REST in Practice](http://restinpractice.com/book/) book and started reading it. I've should done it years ago, instead of arrogantly decide that I got it without looking deeper into the topic. Really, it's more than two years that I build JSON apis saying "yeah, this shit is RESTful!" and no single one of those actually qualifies for that definition.

One more thing I discovered is [Erlang.mk](https://github.com/ninenines/erlang.mk), that I'm trying on another pet project and I think I'll integrate into netronner in the next weeks (no good reason, reading bits and slices of info around the interweb it seems that [Rebar](https://github.com/rebar/rebar) has some issues and this is way more simple and configurable).

Almost nothing to say about the ui, I just added the pagination support to it with the [jQuery waypoints plugin](http://imakewebthings.com/jquery-waypoints/) and made a few fixes. I'm not exactly enthusiast of using more and more jQ on it, but I'm not conviced of the [Angular](https://angularjs.org/)'s way yet and in general lacked the time to work on it more seriously.

Wow, that's a lot of stuff. Another one or two things are that I might bring the achievements list into the git repo as an Erlang terms file (to load with [consult](http://www.erlang.org/doc/man/file.html#consult-1)) to make them part of the release, and I might convert this pile of randomness into a [gitbook](https://www.gitbook.com/). Wouldn't it be interesting to have a continuous evolution/change log in the form of a book?