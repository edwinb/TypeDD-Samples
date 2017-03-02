{-

1. Anything where the list is the same length as the input. We don't know
the types of elements contained in the list, however, so we can't look at them.
Some possibilities (there are many others!):

   - sorting the list
   - reversing the list
   - returning the input unchanged

2. Anything where the output list is twice as long as the input.
Some possibilities:

   - Duplicating each element
   - Returning the list appended to itself

3. Anything where the output list is one item shorter than the input, which
must itself be non-empty. Some possibilities:

    - Returning all but the first element
    - Returning all but the last element

4. The bounded number is likely to refer to a position in the list, so this
could be a function to return the element at that position.

-}
