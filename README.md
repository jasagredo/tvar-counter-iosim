# TVar non-atomic counter examples in IOSim

This repository contains a very simple counter implementation which is racy. It
shows how:

- `IOSimPOR` will find the race always
- `IOSim` will find the race sometimes and sometimes pass, but the outcome is
  reproducible with the same QuickCheck seed, i.e. it is a deterministic
  interpreter.
- `IO` will sometimes find the race and sometimes pass, but the seeds cannot be
  used to reproduce the failure/pass, i.e. it is non-deterministic.
- The same code is used with all these types, by defining it polymorphic on a
  monad `m` which is a instance of some typeclasses (of which both `IO` and
  `IOSim` are instances).

## Example

The implementation is indeed racy because `incr` is not defined as an
`atomically` block but instead as two. So if both threads read the value before
they have time to write to it, they will overwrite each others write operation.

### `IOSimPOR`

First `IOSimPOR` shows this with the following output:

```
    *** Failed! Falsified (after 1 test):
    Schedule control: ControlAwait [ScheduleMod (RacyThreadId [2],2) ControlDefault [(RacyThreadId [1],0),(RacyThreadId [1],1)]]
    Thread {2} delayed at time Time 0s
      until after:
        Thread {1}

    1 /= 2
    Use --quickcheck-replay="(SMGen 12659350400490690812 2043301947758733693,0)" to reproduce.
    Use -p '/IOSimPOR finds race/' to rerun this test only.
```

The output has to be interpreted in the light of the printed trace. It is
essentially saying: if we delay the `thread 2 - step 2` and run if after `thread
1 - step 0` and `thread 1 - step 1` then the property is violated.

If we check the generated trace, we will see that the involved steps are:
```
0s - Thread {1}.1  - Effect VectorClock [Thread {1}.1, Thread [].3] Effect { reads = fromList [TVarId 0] }
...
0s - Thread {2}.2  - TxCommitted [Labelled TVarId 0 TheCounter] [] Effect { writes = fromList [TVarId 0] }
```

So if the write to the TVar is delayed until after the read in the other thread,
then the property is violated.

### `IOSim`

The output is as follows:

```
  IOSim eventually finds race:    FAIL
    *** Failed! Falsified (after 1 test):
    ((0,0),(0,0))
    2 /= 1
    Use --quickcheck-replay="(SMGen 11305134662433048962 15417559823653523023,0)" to reproduce.
    Use -p '/IOSim eventually finds race/' to rerun this test only.
  IOSim eventually finds ok case: FAIL
    *** Failed! Falsified (after 4 tests and 5 shrinks):
    ((0,0),(1,0))
    2 == 2
    Use --quickcheck-replay="(SMGen 11849983721418587666 4047989515431389625,3)" to reproduce.
    Use -p '/IOSim eventually finds ok case/' to rerun this test only.
```

This shows that with the given delays (which have been shrunk if possible), we
can find both cases, one in which the race happens and one in which the property
succeeds.

The interesting thing here is that these seeds can be used to deterministically
reproduce the test case, and the trace can be observed in each case, showing
what is happening on each thread.

It is however not guaranteed that `IOSim` will find the offending sequence of
actions, but if it does find it, it will be reproducible.

### `IO`

The `IO` test case is the non-deterministic one, The properties will sometimes
pass and sometimes fail. Note that the tested function is the one that has no
`threadDelay` so this non-determinism is purely due to the GHC RTS scheduler.

More importantly, there are no generated values, so there cannot even exist
a reproducer for the specific case, the execution in inherently
non-deterministic.

It still serves as an example that the same code can be run with `IO` without
changes.
