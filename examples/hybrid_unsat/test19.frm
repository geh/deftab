classical
consequence:
<>((p1 v p2) & (n1:p3));
[](p1 -> p3);
[](p2 -> p3);
n1:(p3 -> !p1);
n1:<>n2;
n2:((n1:!p1) -> p5);
!(<>p3 v n1:<>p5)
