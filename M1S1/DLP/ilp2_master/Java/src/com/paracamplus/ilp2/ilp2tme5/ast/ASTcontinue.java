package com.paracamplus.ilp2.ilp2tme5.ast;

import com.paracamplus.ilp1.ast.ASTexpression;
import com.paracamplus.ilp2.ilp2tme5.interfaces.IASTcontinue;

public class ASTcontinue extends ASTexpression implements IASTcontinue {

	@Override
	public <Result, Data, Anomaly extends Throwable> Result accept(
			com.paracamplus.ilp1.interfaces.IASTvisitor<Result, Data, Anomaly> visitor, Data data) throws Anomaly {
		// TODO Auto-generated method stub
		return ((com.paracamplus.ilp2.ilp2tme5.interfaces.IASTvisitor<Result, Data, Anomaly>)visitor).visit(this, data);
	}
}