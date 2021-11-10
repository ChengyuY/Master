package com.paracamplus.ilp2.ilp2tme5n.ast;

import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTfactory;

public class ASTfactory extends com.paracamplus.ilp2.ast.ASTfactory 
	implements IASTfactory{

	@Override
	public IASTexpression newBreak(String label) {
		// TODO Auto-generated method stub
		return new ASTbreak(label);
	}

	@Override
	public IASTexpression newContinue(String label) {
		// TODO Auto-generated method stub
		return new ASTcontinue(label);
	}

	@Override
	public IASTexpression newLoop(String label, IASTexpression condition, IASTexpression body) {
		// TODO Auto-generated method stub
		return new ASTloop(label,condition,body);
	}

}