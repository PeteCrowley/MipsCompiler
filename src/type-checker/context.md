# Context stack

Ok, we need some way to map var names to var types.

Because we're in a functional language, we recurse for each additional scope
(and the name->type map is automatically maintained as a stack)

We need 
- some way to add new entries to the map on let
- some way to add a new record on tydec
- to handle recursive records
- and recursive functions
    - mutual recursion is probably going to be really annoying.

I don't think any of this requires anything other than a normal hash table as our contenxt though


