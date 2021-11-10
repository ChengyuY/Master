package com.paracamplus.ilp4.exam1920.compiler.ast;

import com.paracamplus.ilp1.compiler.interfaces.IASTCvariable;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp1.interfaces.IASTvariable;
import com.paracamplus.ilp4.exam1920.ast.ASTforeach;
import com.paracamplus.ilp4.exam1920.compiler.interfaces.IASTCforeach;
import com.paracamplus.ilp4.exam1920.compiler.interfaces.IASTCvisitor;

public class ASTCforeach extends ASTforeach implements IASTCforeach {

	public ASTCforeach(IASTvariable variable, IASTexpression iterator, IASTexpression body) {
		super(variable, iterator, body);
	}

	// simple type refinement
	@Override
	public IASTCvariable getVariable() {
		return (IASTCvariable)super.getVariable();
	}

	@Override
    public <Result, Data, Anomaly extends Throwable> Result 
    accept(com.paracamplus.ilp1.interfaces.IASTvisitor<Result, Data, Anomaly> visitor, Data data)
    throws Anomaly {
        return ((IASTCvisitor<Result,Data,Anomaly>)visitor).visit(this, data);
    }
}
