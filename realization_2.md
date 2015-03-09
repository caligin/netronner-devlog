# Realization of a shitty project, using awesome tech (Part II)

A foreword: someone made me notice that in part I I first wrote "I'm going to pick something new" and then "x because I already know it". Yes, but no: I've seen it, but I'm not familiar with it.

A second foreword: in the end I'm wiriting this issue after 5 months and I don't remember all the details anymore so it's going to be a lot of topics but very briefly.

## Where to start from

So, the development of NetRonner. I had to identify the minimum set of features that could fit a first release to evolve from, in an iterative fashion. The idea of the project was born from our Mahjong achievements, to the very least thing that I might want to do on the platform was being able to browse achievements, award them to players and see which achievements a player earned in his/her gaming career. That seems a good starting point. Small, focused.

So that drove my initial design of the application. Following the "package by feature" approach that we usually adopt in Java, I tried to adapt it to "erlang application by feature". So I initially identified 3 possibly-autonomous applications: `achievements`, `players` and `netronner` (that is the HTTP interface to anything else) that depends on them and serves the single-page UI.

The single-page UI had short life inside the `priv/` folder of netronner as googling around I found out that the [cowboy_static](http://ninenines.eu/docs/en/cowboy/1.0/guide/static_handlers/) handler exists more for development/test purposes than for real-world systems (I think I found it in a 99s presentation but can't find the link again, hope I didn't imagine it). I decided early to completely detach the UI from the API, serving static resources with [nginx](http://nginx.org/). I had three main reasons for this: first, the thing about cowboy_static. Second, I already planned to eventually build an [Andorid](http://www.android.com/) client for NetRonner so the idea of a "pure" API without embedded UI fit better in the picture. Third: I'm toying around with [Docker](https://www.docker.com/), and if a container maps to a service then dividing the platform in an Erlang node container plus an nginx container sounds good.

## Don't ~~cross~~ cors this line!

This was actually a nice experiment as it forced me to struggle with an unexpected guest: [CORS](http://enable-cors.org/). Serving th UI from a different process than the API means different ports... that are considered different origins for the [same-origin policy](http://en.wikipedia.org/wiki/Same-origin_policy). Now I know what virthalhosts and reverse proxies are useful for. However, as I wanted to go on with this separation experiment, I chose to handle CORS in the API instead of coupling UI and API with a reverse proxy. Piece of cake until you have to deal with prefligh, but even that case just boils down to figure which few headers you need to set in your responses and handle the `OPTIONS` verb. Oh, and something like [this](https://twitter.com/Caligin35/status/526845065915006976) may happen, too.

 CORS aside, I have to admit that I enjoyed the clean cut. It helps to keep the focus and actually changes the way you reason about the components as different entities instead of pieces of a single thing that *shoud* not (but indeed could) be tangled together.

## O hai, may I?

One of the parts I hate the most of developing a system where users are supposed to enter data, is when you decide that they need to be authorized to do so. And have to handle some kind of user login. And a registration process maybe. But all the information that you keep about your users is usually just overhead and a distraction, how to deal with it?

The easy choice might have been a single password for everyone. Since I see the user base every Tsumonday at my place, estabilishing a shared secret with everyone is a piece of cake. But when I award an achievement on the platform I need a way to identify who I'm awarding it too. That makes a "player" entity a first-class element indeed.

Well the choice for this was actually easier than expected: we use a private [g+](https://plus.google.com/) community to discuss about our Riichi sessions, that means (almost) everyone has a g+ account. So I researched [how to add a g+ signin button](https://developers.google.com/+/web/signin/add-button), [OAuth 2.0](http://oauth.net/2/) and [google APIs console](https://console.developers.google.com). The design I came up with is the following:
- UI obtains the OAuth token from google. This allows to choose the achievement assignee among people in your circles.
- When UI wants to award an achievement, it authenticates with the [bearer token](https://tools.ietf.org/html/rfc6750) mechanism.
- API can ask google apis if the token is a valid token, released for its key.
- API can also use the token to ask google some basic information about the player to allow some visualization later (name and icon, mainly).
- If it is, user is authenticated and *should* be authorized to award.

I say should because I can't have anyone on g+ awarding stuff to anyone else. I ony own a small server, I don't have the resources (nor intend to acquire them yet) to handle a possible overgrowth in the user base. So to actually be authorized, an authenticated user will either need to be me (yeah, I hardcoded my g+ id) or have an achievement already. This is also more fitting of our community's model: we're not a club, we're just a bunch of people trying to spread the game.

One last point I have to make is about security: these tokens have a validity of 1 hour, so they're subject to replay attacks. This means that HTTPS is mandatory to prevent a malicious party from acquiring the tokens.

## People don't like to be listed

First mock design was something like a list of players, each one with the achiavements he/she unlocked. Trying to replicate it, I hit the [Riak](http://basho.com/riak/) wall: if every player is a document in a `players` bucket, how should I list them all?
Riak handles badly a full-bucket scan (or so says the documentation), and as I said in the first post I wanted to avoid second-level indexes in order to feel advantages and limitations of a K-V model. Guess this is one. I may use a special document with the list of all the accessible keys, but it may grow undefinetly along with users.

This called for a big design change. Now that I think of it, isn't this a problem for facebook, twitter, g+ and any other social network too? How do they cope with that? Oh, I see. There's no place in neither of three where you can see all the users. You can search with criterions, load by id, list a small category (e.g: my friends) but not list all of them. And you don't need to do so.

What did I want to show on the interface, if not a list of players with their shining achievement plates? Maybe something more event-based? Like "Hey everyone, look who just scored a full flush!", and then being able to see the detail of this lucky player. Sounds good, and this allows me to actually load players instead of list them. Screw list. Achievement awards shall generate events to be shown on a timeline, and a timeline is reasonably paginated. Implementing a new-page-once-previous-filled pagination mechanism with Riak is quite straightforward, and allows to load pages that eventually lead to the discovery of the id for the next one. Very similar to [Atom](http://tools.ietf.org/html/rfc4287)'s live and archive feeds. All this logic is contained in the `timeline` application with the introduction of a form of event bus to handle inter-application (read as inter-feature) communication.

For a this design in the UI I tried to mimick the simplicity of [Twitter](https://twitter.com/)'s interface.

## Driving is awesome

A quite pointless note: the achievement list was on a google drive spreadsheet, had to find a way to export to JSON, discovered that spreadsheets are scriptable with Javascript and it's awesome.

## Summing it up

The initial design process was long and filled with new stuff, unexpected obstacles and design changes even if the application is extremely small. What I reported here is very brief but I think the important thing I learnt is that developing alone, leaving the thoughts without rein and way too much degrees of freedom, leads to a complicated design process even without complicating the architecture. There are many options for which the tradeoffs are totally unclear and without someone else to pair with the only feedback is by actually trying and see what happens, possibly after having wasted hours in an attempt to determine the best option from the description alone.

Enough for tonight, see ya next!
