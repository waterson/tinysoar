tinysoar
========

TinySoar is an implementation of the [Soar](http://ai.eecs.umich.edu/soar)
artificial intelligence architecture that is intended to run on a
memory-constrained device, like a robot. Soar is a "real time" (as opposed to "off-line")
performance runtime that incorporates acting, planning, and learning
in a rule-based framework.

TinySoar consists of two primary components. The first is a portable,
light-weight runtime that implements the Soar decision cycle as a host
for a Soar agent. The second is a Tcl extension that is used to create
and debug Soar agents, and then export them into a format that can be
compiled into the runtime component.

A sample embedding (in fact, the one that inspired the project!) is
also included: alternative firmware for the Lego® Mindstorms RCX 2.0
(the old yellow one) that embeds the TinySoar runtime. Using this firmware,
you can control a
Lego robot with a Soar agent. (Disclaimer: this site is neither
endorsed by nor associated with Lego. I'm telling you this because
every other Lego Mindstorms site does the same.)
