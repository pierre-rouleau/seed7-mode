============================================================
Seed7 Mode — Indentation Performance & Correctness Benchmark
============================================================

:Running with    : seed7-mode 2026-07-01T17:44:07+0000 W27-3
:Input dir       : /Users/roup/my/dvo/seed7-repos/seed7-t1/
:Output dir      : /Users/roup/my/tmp/seed7/indent-test/18.1a/
:Generated on    : 2026-07-02T18:46:18-0400
:Files processed : 8
:Files succeeded : 8
:Total indent time: 3622.437 s
:Wall-clock time   : 60:22.479
:Profiling         : disabled

Correctness Check
=================

To check whether re-indentation changed any file, run::

  diff -r /Users/roup/my/dvo/seed7-repos/seed7-t1/ \
          /Users/roup/my/tmp/seed7/indent-test/18.1a/

No output from ``diff -r`` means seed7-mode indentation is idempotent
for all files in the input tree.

To compare two OUTPUT-DIRs produced by different versions of seed7-mode::

  diff -r OUTPUT_DIR_BEFORE OUTPUT_DIR_AFTER

Any line in the diff output indicates a change in indentation decisions.

File Indentation Times
======================

================================================== ======= ================
File Name                                                5  Indent Time (s)
================================================== ======= ================
prg/addup.sd7                                          190 0.205830
prg/bas7.sd7                                         11459 3425.616865
prg/bifurk.sd7                                          73 0.048637
prg/bigfiles.sd7                                       129 0.119938
prg/brainf7.sd7                                         86 0.088468
prg/calc7.sd7                                          128 1.393823
prg/carddemo.sd7                                       190 0.171351
prg/castle.sd7                                        3148 194.792009
================================================== ======= ================

Statistical Summary
===================

Per-file `indent-region' times (seconds).

+-----------+------------------+
| Metric    | Time (s)         |
+===========+==================+
| Files     | 8                |
+-----------+------------------+
| Average   | 452.804615       |
+-----------+------------------+
| Minimum   | 0.048637         |
+-----------+------------------+
| Maximum   | 3425.616865      |
+-----------+------------------+
| Total     | 3622.436921      |
+-----------+------------------+


.. ---------------------------------------------------------------------------
