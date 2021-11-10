package com.paracamplus.ilp4.exam1920.interfaces;

import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp1.interfaces.IASTvariable;

public interface IASTfactory extends com.paracamplus.ilp4.interfaces.IASTfactory {
	IASTforeach newForeach(IASTvariable variable,  IASTexpression iterator, IASTexpression body);
}
