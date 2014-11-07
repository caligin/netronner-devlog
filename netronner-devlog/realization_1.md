# Realization of a shitty project, using awesome tech (Part I)

Hi everyone, Caligin here!

This serie of articles will be the development log of Netronner, a web application to add some fun to real-life mahjong, first but not last the ability to award achievements for our gaming feats.
I'll try to capture here decisions, challenges, pitfalls and whatever else I encountered during the development of this platform.

First thing: why the article is called like this? Only an idiot would develop (in his free time) something he thinks is shitty.
Well the answer is easy: when I started, I choose the tech stack. I expected the project to be something with quite easy logics, and regarded it as a good playground to learn something new. I could have implemented this with Java+Spring+PSQL/Mongo (the stack I usually work with) in maybe two hours, but I chose not to.
Then in an Hangout with Angelo, while talking about the project, he said "You know what? You cold write a thesis on this. Something with a title like 'Realizzazione di un progetto del cazzo, utilizzando tecnologie fighissime.'" -- that's the italian for 'Realization of a shitty project, using awesome tech' -- "I'm sure the professors would like it." (Just to be clear, he was being sarcastic. Our professors are all very old and not inclined to accept any kind of informal interaction). I decided to quit the university just a few days before (Trieste is no good for CS, and right now I have better options than moving to another place like Milan), but I liked the title and so I decided to use it for a serie of articles. So here I am.

Now on to business. I'm writing this first article at the end of the first iteration, so it might result long and poorly detailed. I wanted to write little by little, but I really lacked the time. And I was eager to get my hands dirty and build this toy.

First thing, the stack.
I went for erlang. I had the chance to learn it while developing the (Muppetforge)[https://github.com/closure-science/muppetforge], but since I pretty much followed rferranti during that project, I had the feeling that I need to build something on my own to consolidate what I learnt. Also, I have a very small virtual box available to deploy, so I wanted something that could operate with small memory footprint. A Java webapp is a no-no for a machine with less than 1GB ram (Actually, I still have to measure this aspect, this statement is made out of very unscientific spannometry and mechanical sympathy).

With Erlang as the language, the web framework had to be Cowboy. I never tried Mochiweb or... uh, can't remember the name of the other one. But I already knew a little about Cowboy, I liked it and reading Essen's Twitter feed I have trust in the choices he makes evolving it. It doesn't have all the fancy automagic that Spring Web provides, but it's simple, transparent and you can build on it much what and how you like.

Let's see, what else do I need? Ohya, a buid system and a VCS. Rebar + Erlware's makefile felt easy and painless the last time, also I have the feeling that's what people usually go for in the Erlang/OTP community (am I wrong?). As for the VCS, git it is.
I really prefer the simplicity of Mercurial, but the ecosystem of and around Github is definetly better that Bitbucket. It's a good chance to learn some git too (And I have to say here, after a few months I'm still at a loss on everything that goes beyond push/pull/commit/reset working copy to a clean state).

Next, I need a database. I excluded Mongo and Postgres as they're not new to me. And I wanted something that could feel natural in Erlang, so the tie was between Mnesia/ETS/DETS and Riak. I went for Riak. Why? Because fuckyou, that's why LOLOLOLOLOL! No ok, jokes aside there's no good technical reason for that. I'm still unsure Riak was a good choice but I was courious about how you model data when you have a KV database under your ass (this means I'm avoiding 2nd level indexes on purpose, the backend is Bitcask and I'll stick to it). Right now I'm considering the possibility of switching back to Mnesia, I have the feeling that it might be easier and a better/more useful study.

As for the GUI, I wanted a single-page application written with something simple. I somehow forgot about looking at angular.js so I ended up with jQuery+Bootstrap. I want to use jQ the less I can but for just a couple of content replacements, a few ajax requests and nothing more it's a good fit. Oh, I almost forgot: Handlebars for some templating. All these picks are just me being lazy (Bootstrap aside, I knew very little about its grid system) since it's exactly the same stack used on the Muppetforge. I followed the angular tutorial only last week and I'll replace the current stack with it for sure in the next weeks.

Aaaaaaaaaand... Yes, I think that's all for the tools. I'll stop here for now, I'll talk about the first steps of the development next time.
