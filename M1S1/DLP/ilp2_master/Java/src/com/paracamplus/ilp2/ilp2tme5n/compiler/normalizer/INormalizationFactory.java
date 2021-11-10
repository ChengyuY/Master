package com.paracamplus.ilp2.ilp2tme5n.compiler.normalizer;

import com.paracamplus.ilp1.interfaces.IASTexpression;

 public interface INormalizationFactory 
 	extends com.paracamplus.ilp2.compiler.normalizer.INormalizationFactory {
    

     IASTexpression newContinue(String label);

     IASTexpression newBreak(String label);
     
     IASTexpression newLoop(String label, IASTexpression condition, IASTexpression body);
     

}