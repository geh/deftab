intuitionistic
defaults:
  T --- T --> !(P1 & P2); 
  T --- T --> !(P1 & P3); 
  T --- T --> !(P2 & P3); 
consequence:
  !((P1 & P2) v (P2 & P3) v (P1 & P3))
