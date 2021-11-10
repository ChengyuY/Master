package com.paracamplus.ilp4.exam1920.ast;

import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp1.interfaces.IASTvariable;
import com.paracamplus.ilp4.exam1920.interfaces.IASTfactory;
import com.paracamplus.ilp4.exam1920.interfaces.IASTforeach;

public class ASTfactory extends com.paracamplus.ilp4.ast.ASTfactory
		implements IASTfactory {

	@Override
	public IASTforeach newForeach(IASTvariable variable, IASTexpression iterator, IASTexpression body) {
		return new ASTforeach(variable, iterator, body);
	}

}
