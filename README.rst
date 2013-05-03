Random123
=========

This is a Haskell port of counter-based random number generators from `Random123 library <http://www.thesalmons.org/john/random123/>`_ v1.07 (with a minor bugfix).
The description of algorithms can be also found in `Salmon et al., P. Int. C. High. Perform. 16 (2011) <http://dx.doi.org/doi:10.1145/2063384.2063405>`_.


TODO
----

* Performance issues:

    * According to Salmon et al., Threefry-4x64 should be the fastest algorithm on CPUs.
      This is what not I'm seeing; need to investigate it further.
      If it is made faster, it should be used as the default bijection for ``CBRNG32/64``
      instead of ``philox4``.

    * ``mulhilo`` function in 64-bit Philox is not optimal in terms of performance.
      It can be made faster given the access to CPU intrinsics.

    * 32-bit Threefry shows suprisingly low performance (see ``Bijection`` benchmark group).

    * In general, there seems to be a lot of optimizations that can be done,
      in particular in terms of strategically placed strictness enforcement.
