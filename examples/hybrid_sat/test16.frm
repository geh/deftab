classical
consequence:
n1:(p1 -> p2);
n2:(p2 -> p3);
n3:(p3 -> p4);
p4 -> (n1: n2);
p2 -> (n1: n3);
[][][]false;
<><>(p1 v false -> p3);
<><>(p2 -> false v [](p1 <-> n3));
<>(<>p1 v []p3) <-> <>(<>p3 v <>p2)
