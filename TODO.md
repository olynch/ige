Milestones
---
- [X] Display a node (circular or rectangular)
- [X] Display a node with text in it
- [X] Display multiple nodes in various places
- [X] Display a straight edge
- [X] Display a straight edge with an arrow head
- [X] Display a spline edge with an arrow head
- [X] Display nodes and edges together, with manual positioning
- [X] Write algorithm for automatic positioning (force-directed graph drawing?)
- [ ] Create serialization format and implement serialization and deserialization
- [X] Implement command line mode (ie. :, like in vim)
- [ ] Write commands for selecting, adding, editing, and deleting nodes
  - [X] Adding
  - [X] Deleting
  - [ ] Selecting
  - [ ] Editing
- [ ] Write commands for selecting, adding, editing, and deleting edges
  - [X] Adding
  - [ ] Editing
  - [ ] Deleting
  - [ ] Selecting
- [ ] Create keyboard shortcuts with arpeggio tree (ala vim or spacemacs, like c-a-n: change a node, or whatever)
- [X] Write selecting interface similar to vimium's "f"
- [ ] Write msgpack communication between Haskell and Rust
  - [ ] write msgpack API, implement using rmp-serde and messagepack


Things We Have Learned
---
Conduit is awesome -- we should use conduit on the haskell side to implement the
entire interaction.
- Reading from the socket is a Conduit
- Parsing msgpack data is a Conduit
- Processing Events and sending Commands is a Conduit
- Serializing msgpack data is a Conduit
- Writing to the socket is a Conduit

Stream abstractions in general are great (using Read and Write on Rust, along
with `rmp_serde::Deserializer::from_read`.

Unix sockets and messagepack are the way to go.

Threads are little fluffy bunnies in Haskell. Use forkIO to spawn them, and use
STM for communication.

Next Steps
---
It seems like a refactor is in order.

Write message processing on the Haskell side -- a Conduit from Events to
Commands
- How can we make keyboard mappings and chords loosely coupled from one massive
  case statement?
- We should bring in megaparsec for parsing cmdline commands
- How can we implement stuff like loading files, etc.?

Write message processing on the Rust side -- a little more complicated, as we
need to asynchronously send Events as we receive keyboard input, and Events and
Commands are not coupled the way they are on the Haskell side.
- Does deserializing thread put Commands in a channel or process them directly?
  - Different Commands will operate at different levels (ie. graph, layout,
    view, etc.) -- how do we do this?

Rename Event::Command or rename Command.

