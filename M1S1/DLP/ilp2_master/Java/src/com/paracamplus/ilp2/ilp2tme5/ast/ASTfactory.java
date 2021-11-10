package com.paracamplus.ilp2.ilp2tme5.ast;

import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp2.ilp2tme5.interfaces.IASTfactory;

public class ASTfactory extends com.paracamplus.ilp2.ast.ASTfactory 
	implements IASTfactory{

	@Override
	public IASTexpression newBreak() {
		return new ASTbreak();
	}

	@Override
	public IASTexpression newContinue() {
		return new ASTcontinue();
	}

}