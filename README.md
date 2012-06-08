# mochicow

mochicow is a mochiweb adapter for cowboy.

There are 2 ways to use mochicow:

- as a cowboy protocol: It will use the socket acceptor pool of cowboy
  instead of the mochiweb one.
- as a protocol upgrade. Like websockets you can upgrade a cowboy
  handler to use a mochiweb loop. It allows you to use both your new
code with cowboy and old code with mochiweb.


See the examples in the `examples` for the usage.
