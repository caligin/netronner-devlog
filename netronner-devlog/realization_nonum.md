# Realization of a shitty project, using awesome tech (Part Idontknow)

Ehrm, this is a bit in a hurry. This is not part 2, it's about where I am right now.
The point is that I don't want to leave thoughts in the air and then struggle to remember them months later when I write like I did for the rest.
I swear I'll reorder everything once I catch up.

Anyway: I'm at the end of the first development cycle.
Netronner *should* work.
Yes, *should*. I have almost no confidence in it. The last year was like a making steps backwards for me, for reasons. So I lost the discipline of testing regularly, lost the enthusiasm to pick and learn all the correct tools before creating something. That's to say, I didn't write a single test on this project. And neither I did on KancolleCommander (something I didn't publish yet... Basically it's an android porting of Kantai Collection). And I don't test things at work too, lately.

Why? First of all, it feels kinda stupid to test glue. Most of the things that I write are just glue between well-tested components, which I can rely on.
Second: yes some of that glue is actually business logic and should be tested. But when there's a database involved I start to spit out pathetic reasons to avoid testing it. It's mostly lazyness, fear of the dirty state and indecision about when to mock it and when to write a test with the real db.
Third: I was too lazy to better understand how to do good dependency in Erlang.
Fourth: I have no idea about how to mock the Google OAuth api. No this is not quite right... I's actually easy for netronner-api, it's just a cople of https calls. It might be sensibly harder for the UI though, part of the flow is automagically handled by the "g+ Login" button.
Long story short: no tests, no good DI, no good tools, no idea about how to handle UI dependencies, no mocks, no good mechanism to inject credentials in the build.

Yes, I'm not proud of that. I partly wanted to reach a first release and then polish while deploying continuously but in the end I just slowed down the entire process.

So now it's deploy time and there are still a lot of questions without an answer (and a long list of TODO saved in my google keep).
Then? Then nothing, I must stop and polish everything. But right now, release is the word. The first user-base are just us, so even if it's not perfect nobody's gonna die. And I can have feedback.

Then, how to deploy?
Docker. Docker because it's easy, because I don't like Puppet, because it's one of the latest big things, because I have the feeling that it will prove useful both now and in future and it's a good investment learning it.

I installed Docker on Iris (one of my machines) right now, and the first step towards the deployments is to dockerize the toy. I'm unsure about using one or two containers (api + ui), I guess it depends on where I'm going to deploy it. I have a virtual box but I can't install docker on it, so I'm going to look for a docker hosting service.

After that, I can start the refactoring/reorganization of all the codebase and tools. And start the Andorid part of the project too, as I am discussing right now with Angelo.

Ok this is it for now, I'll post an update when things are ready!