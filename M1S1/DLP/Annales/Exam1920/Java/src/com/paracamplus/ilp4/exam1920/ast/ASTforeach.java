package com.paracamplus.ilp4.exam1920.ast;

import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp1.interfaces.IASTvariable;
import com.paracamplus.ilp1.interfaces.IASTvisitor;
import com.paracamplus.ilp4.exam1920.interfaces.IASTforeach;

public class ASTforeach implements IASTforeach {

	private IASTvariable variable;
	private IASTexpression iterator;
	private IASTexpression body;
	
	public ASTforeach(IASTvariable variable,  IASTexpression iterator, IASTexpression body) {
		this.variable = variable;
		this.iterator = iterator;
		this.body = body;
	}	
	
	@Override
	public <Result, Data, Anomaly extends Throwable> Result accept(IASTvisitor<Result, Data, Anomaly> visitor,
			Data data) throws Anomaly {
		return ((com.paracamplus.ilp4.exam1920.interfaces.IASTvisitor<Result,Data,Anomaly>)visitor).visit(this, data);
	}

	@Override
	public IASTvariable getVariable() {
		return variable;
	}

	@Override
	public IASTexpression getIterator() {
		return iterator;
	}

	@Override
	public IASTexpression getBody() {
		return body;
	}

}
