package com.paracamplus.ilp4.exam1920.interfaces;

import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp1.interfaces.IASTvariable;

public interface IASTforeach extends IASTexpression {
	IASTvariable getVariable();
	IASTexpression getIterator();
	IASTexpression getBody();
}
