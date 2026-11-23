# Comparison of last two versions

Here are the results of a comparative analysis of Kupo 2.11.0 (control) and
Kupo 2.12.0 (experimental). The experiment was done twice.

Date: 2026-07-22.

First time:
```
Comparing:
data/20260722-070332 (s1: experimental group)
data/20260722-065951 (s2: control group)
Common datasets:
1 2 3 4 5 6 7 8
Significance level: α = 0.05
1: errors (s1/s2): (0/0); comparison: not significant
2: errors (s1/s2): (0/0); comparison: not significant
3: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.0916194901341525
4: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.0535370652888765
5: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.1398644489218612
6: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.5449497464477424
7: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.1963714961632728
8: errors (s1/s2): (0/0); comparison: s1 faster by factor 4.8343761442035224e-4
```

Second time:
```
Comparing:
data/20260722-071609 (s1: experimental group)
data/20260722-071055 (s2: control group)
Common datasets:
1 2 3 4 5 6 7 8
Significance level: α = 0.05
1: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.100916485402807
2: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.0708009466150419
3: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.0582589486699308
4: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.058555515473119
5: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.084723814838676
6: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.2807685516569087
7: errors (s1/s2): (0/0); comparison: not significant
8: errors (s1/s2): (0/0); comparison: s1 faster by factor 4.539446387978876e-4
```

This shows fairly consistently that in general, the new version is very
slightly slower, but specifically in the case of dataset 8 the new version is
blazingly faster (cf. [Issue 194][issue194]).

[issue194]: https://github.com/CardanoSolutions/kupo/issues/194
