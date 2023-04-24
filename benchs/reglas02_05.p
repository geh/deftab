intuitionistic
defaults:
  T --> !(P1 & P2); 

  T --> !(P1 & P3); 
  T --> !(P2 & P3); 

  T --> !(P1 & P4); 
  T --> !(P2 & P4); 
  T --> !(P3 & P4); 

  T --> !(P1 & P5); 
  T --> !(P2 & P5); 
  T --> !(P3 & P5); 
  T --> !(P4 & P5); 

consequence:
  !(  (P1 & P2)
    v (P2 & P3) v (P1 & P3)
    v (P1 & P4) v (P2 & P4) v (P3 & P4)
    v (P1 & P5) v (P2 & P5) v (P3 & P5) v (P4 & P5))
