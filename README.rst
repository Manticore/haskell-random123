Random123
=========

This is a Haskell port of counter-based random number generators from `Random123 library <http://www.thesalmons.org/john/random123/>`_ v1.07 (with a minor bugfix).
The description of algorithms can be also found in `Salmon et al., P. Int. C. High. Perform. 16 (2011) <http://dx.doi.org/doi:10.1145/2063384.2063405>`_.


Contributing
------------

When making changes to the library, run (or update, if necessary) functionality tests.
This can be done as

::

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test

or just by executing ``cd test; ./test.sh``.
You can also check the performance by running benchmarks as

::

    $ cabal configure --enable-benchmarks
    $ cabal build
    $ cabal bench

or by executing ``cd test; ./test_perf.sh``.
Benchmarks will create a report file ``test_perf.html``
in the folder where they were executed from.


TODO
----

* Performance issues:

    * According to Salmon et al., Threefry-4x64 should be the fastest algorithm on CPUs.
      This is not what I'm seeing; need to investigate it further.
      If it is made faster, it should be used as the default bijection for ``CBRNG32/64``
      instead of ``philox4``.

    * ``mulhilo`` function in 64-bit Philox is not optimal in terms of performance.
      It can be made faster given the access to CPU intrinsics.

    * 32-bit Threefry shows suprisingly low performance (see ``Bijection`` benchmark group).

    * In general, there seems to be a lot of optimizations that can be done,
      in particular in terms of strategically placed strictness enforcement.

* Current ``split`` implementation is a quick solution that kind of works
  (much like ``StdGen``'s one).
  A mathematically robust implementation is required
  (and CBRNGs by nature should be well-suited for this).
  Moreover, it would be great to have some tests that could distinguish
  "bad" ``split`` from a "good" one.
