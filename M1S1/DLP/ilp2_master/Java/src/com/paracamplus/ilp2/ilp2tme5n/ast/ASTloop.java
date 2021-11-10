package com.paracamplus.ilp2.ilp2tme5n.ast;

import com.paracamplus.ilp1.annotation.OrNull;
import com.paracamplus.ilp1.ast.ASTexpression;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTloop;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTvisitor;

public class ASTloop extends ASTexpression 
implements IASTloop {
    
    public ASTloop (String label,IASTexpression condition, IASTexpression body) {
    	this.label = label;
        this.condition = condition;
        this.body = body;
    }
    private String label;
    private final IASTexpression condition;
    private final IASTexpression body;
    
    @Override @OrNull
    public String getLabel() {
    	// TODO Auto-generated method stub
    	return label;
    }

    @Override
	public IASTexpression getCondition() {
        return condition;
    }

    @Override
	public IASTexpression getBody() {
        return body;
    }

   @Override
	public <Result, Data, Anomaly extends Throwable> Result accept(
			com.paracamplus.ilp1.interfaces.IASTvisitor<Result, Data, Anomaly> visitor,
			Data data) throws Anomaly {
		return ((IASTvisitor <Result, Data, Anomaly>) visitor).visit(this, data);
	}

}
