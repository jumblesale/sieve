# SIEVE

A very verbose implementation of the sieve of Eratosthenes.

## Running

The whole thing runs through `ghci`:

```
λ: :l sieve.hs
[1 of 1] Compiling Main             ( sieve.hs, interpreted )
Ok, modules loaded: Main.
λ: sieve 100
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
λ:
```

## How it works

This is a pretty standard implementation of the sieve of Eratosthenes. The `sieve` function
 maintains a dictionary (`Data.Map`) of all the numbers between 2 and the limit and a boolean
 value representing if this number has been proven to be non-prime (`False` means this is not a prime).
 This is initially set to `True` for all values.

 Then the function produces all multiples of 2 and updates the map to set these multiples to `False` -
 we know these can't be primes. 2 is added to a second list of 'visited' numbers. Then the function
 finds the first number `> 2` which has not been disproven as a prime. This gives 3. The function
 updates the map with all multiples of 3 then takes the next non-prime, unvisited number, 5. This
 continues until it hits the terminating clause - when the next number to check has a square `>` the
 limit.

## Tests

Most methods have a `quickCheck` style test! All the `prop_*` bits under the method
 describe how it should work. You can inspect them all with the `testReport` method
 which gives you a nice bit of formatted output.

## Performance

On my 2015 Macbook Pro this can generate ~30,000 primes instantly. I've tested up to 200,000
 which takes a few seconds. The improvements section contains details on how this could be
 better.

## What went well

* The baked-in tests are fun and give immediate feedback on if a function is behaving as expected.
* Generating the initial `Map` with `(Map.fromList $ zip ([2,3..limit]) (repeat True))` is a really
  nice way of expressing the list.
* Seems to perform pretty well!

## What did not go so well

* The tests are hard to read and make it look untidy. Also every `prop_` has to get duplicated in the
  `tests` list. I didn't want to use a framework for this so the messy implementation seemed good enough
  without having to include any dependencies.
* Formatting - the spec asked for a multiplication table but this just dumps a list of numbers. I didn't
  have enough time to tackle this properly - what would have been a few lines in Python feels like it
  would have been a lot of hassle in Haskell.
* `markIndexesAsNonPrime` is clearly a fold but I could not get this to play nicely. If somebody wanted
  to PR this I'd be really interested to see how I should have gone about it.
* Using actual `quickCheck` allows you to run arbitrary data through a function under test. I could not
  get anything useful out of this but given more time would love to explore it further.
* I started off having a single list of numbers which I was dropping elements of as I discovered they
  were not primes. This solution quickly became umanageable but if you would like to see it it is in
  the git history. The choice of `Data.Map` was clearly easier to work with than just using `[Int]`s.
* This probably looks horrible to a Haskell programmer. I'm still new to the language and it was
  fun to have a problem to solve using it. Hoepfully some of the stuff in here is idiomatic but
  I suspect there's a much neater way of doing, for instance, `sieveIteration :: Int -> Int -> [Int] -> Map Int Bool -> Map Int Bool` which I do not yet know about.

## Improvements

[Haskell Wiki](https://wiki.haskell.org/Prime_numbers) has a whole page on this which I resisted reading.
 Their expression of the problem is extremely concise:

```
primesToQ m = eratos [2..m]
  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p..m])
```

Which is what `sieve.hs` expresses in 81 lines. If my solution has any benefit over the Haskell Wiki version
 it is hopefully that it clearly shows how I arrived at a solution by decomposing the problem into
 sub-problems and then using higher order functions to solve the problem. The Haskell Wiki solution remains,
 to me, essentially magic.
