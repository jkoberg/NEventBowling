NEventBowling
=============

Another iteration of the Bowling kata. 

This time with EventStore and CQRS, in F# functional style.

Commands are applied to the game aggregate, and the resulting events are persisted. The even stream is then used to 
update the state of the aggregate model.


