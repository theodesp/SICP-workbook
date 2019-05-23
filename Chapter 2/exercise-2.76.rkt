#lang racket

;Exercise 2.76: As a large system with generic operations evolves, new types of data objects or new operations may be needed. For each of the three strategies—generic operations with explicit dispatch, data-directed style, and message-passing-style—describe the changes that must be made to a system in order to add new types or new operations. Which organization would be most appropriate for a system in which new types must often be added? Which would be most appropriate for a system in which new operations must often be added?

In message-passing we only need to add a new operator type. This is like a type switch so it's easy to add new types.

In explicit dispatch we manualy need to add transformations so it's only suitable if we dont want many representations usually 2 or 3.

In data-directed style we look at the data and add appopriate installers for each dataset. It is more flexible that the explicit dispatch but more clunky than message-passing.
