# KeyedList library for Elm

Implements some Dict functions on top of a list.
Useful for building UI's of collections
of elements that can be updated individually and
added, removed, or reordered. This is implemented
using UID's, NOT the position of each element, so
messages always go to the right place even if the list
is reordered.

Unfortunately there is no one Elm data structure that
can handle this efficiently, but at least List is
okay at iteration and mapping, and plays well with Html.

## Alternative Implementations Considered

**Array:** Great at updating individual elements,
inefficient to map to HTML. Also, uses position-based
keys. We need UID's.

**Dict:** Would be perfect, but it doesn't preserve order.

Combining these wouldn't make sense, because you
just get the worst of both worlds!
