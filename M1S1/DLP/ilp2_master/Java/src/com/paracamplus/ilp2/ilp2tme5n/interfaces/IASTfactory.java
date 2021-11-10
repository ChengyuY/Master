package com.paracamplus.ilp2.ilp2tme5n.interfaces;

import com.paracamplus.ilp1.interfaces.IASTexpression;

public interface IASTfactory extends com.paracamplus.ilp2.interfaces.IASTfactory {
    
    IASTexpression newBreak(String label);
    IASTexpression newContinue(String label);
    IASTexpression newLoop(String label, IASTexpression condition , IASTexpression body);
}