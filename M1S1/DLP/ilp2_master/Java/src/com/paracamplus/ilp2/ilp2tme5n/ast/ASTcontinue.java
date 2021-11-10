package com.paracamplus.ilp2.ilp2tme5n.ast;

import com.paracamplus.ilp1.annotation.OrNull;
import com.paracamplus.ilp1.ast.ASTexpression;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTcontinue;

public class ASTcontinue extends ASTexpression implements IASTcontinue {

	private String label;
	
	public ASTcontinue (String label) {
		this.label = label;
	}
	
	@Override @OrNull
	public String getLabel() {
		return label;
	}
	
	@Override
	public <Result, Data, Anomaly extends Throwable> Result accept(
			com.paracamplus.ilp1.interfaces.IASTvisitor<Result, Data, Anomaly> visitor, Data data) throws Anomaly {
		// TODO Auto-generated method stub
		return ((com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTvisitor<Result, Data, Anomaly>)visitor).visit(this, data);
	}
}