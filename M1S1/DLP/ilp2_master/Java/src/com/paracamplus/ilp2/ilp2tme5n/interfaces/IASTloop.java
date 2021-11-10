package com.paracamplus.ilp2.ilp2tme5n.interfaces;

import com.paracamplus.ilp1.interfaces.IASTexpression;

public interface IASTloop extends IASTexpression {
	String getLabel();
	IASTexpression getCondition();
	IASTexpression getBody();
}