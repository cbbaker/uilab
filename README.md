# Front-end as infrastructure

We've gotten pretty good at splitting web back-ends into decoupled
services, and front-ends have become the new monoliths. The goal of
UILab is to explore pushing as much application logic to the back-end
as possible.

In this architecture, there are three different types of UI
components: *layouts*, *choices*, and *panes*. Layouts and choices have
children; panes do not. UI elements can communicate via a pub/sub
mechanism that works like a websocket, but without a backend.

A *layout* is product of UI elements. It has child elements, and it
always displays them all. The Layout module handles all of the
boilerplate for dealing with children. Different layouts just have
different view functions, which generate the html.

A *choice* is like a layout, but only one child is visible at a time. It
is a sum of UI elements. It subscribes to a channel that tells it
which child to display.

Most of the UI work goes into *panes*. Panes are the visual
representation of the objects in your application domain. They are
composed into layouts and choices.

This is intended to be a proof of concept. If this architecture is
successful, the reusable components will be extracted into
libraries. Comments are welcome :)




