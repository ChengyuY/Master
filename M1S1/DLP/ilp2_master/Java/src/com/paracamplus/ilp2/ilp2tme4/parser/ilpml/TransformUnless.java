package com.paracamplus.ilp2.ilp2tme4.parser.ilpml;

import com.paracamplus.ilp1.interfaces.IASTalternative;
import com.paracamplus.ilp1.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp1.interfaces.IASTblock;
import com.paracamplus.ilp1.interfaces.IASTboolean;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp1.interfaces.IASTfloat;
import com.paracamplus.ilp1.interfaces.IASTinteger;
import com.paracamplus.ilp1.interfaces.IASTinvocation;
import com.paracamplus.ilp1.interfaces.IASTsequence;
import com.paracamplus.ilp1.interfaces.IASTstring;
import com.paracamplus.ilp1.interfaces.IASTunaryOperation;
import com.paracamplus.ilp1.interfaces.IASTvariable;
import com.paracamplus.ilp1.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp2.ilp2tme4.ast.ASTfactory;
import com.paracamplus.ilp2.ilp2tme4.interfaces.IASTfactory;
import com.paracamplus.ilp2.ilp2tme4.interfaces.IASTunless;
import com.paracamplus.ilp2.ilp2tme4.interfaces.IASTvisitor;
import com.paracamplus.ilp2.interfaces.IASTassignment;
import com.paracamplus.ilp2.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp2.interfaces.IASTloop;
import com.paracamplus.ilp2.interfaces.IASTprogram;

public class TransformUnless implements IASTvisitor<IASTexpression,Void,EvaluationException>{
	
	IASTfactory f = new ASTfactory();
	
    public Object visit(IASTprogram iast) 
            throws EvaluationException {
    	int length = iast.getFunctionDefinitions().length;
    	IASTfunctionDefinition[] functions = new IASTfunctionDefinition[length];
    	System.out.println("program visit!");
        try {
            for(int i = 0;i < length;i++) {
            	functions[i] = iast.getFunctionDefinitions()[i];
            }
            return f.newProgram(functions, iast.getBody().accept(this, null));
        } catch (Exception exc) {
            return exc;
        }
    }

	@Override
	public IASTexpression visit(IASTassignment iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("assignment visit!");
		return f.newAssignment(iast.getVariable(),iast.getExpression().accept(this,data));
	}

	@Override
	public IASTexpression visit(IASTloop iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("loop visit!");
		return f.newLoop(iast.getCondition().accept(this, data),iast.getBody().accept(this,data));
	}

	@Override
	public IASTexpression visit(IASTalternative iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("alternative visit!");
		IASTexpression condition = iast.getCondition().accept(this, data);
		System.out.println("condition visit!");
		
		IASTexpression consequence = iast.getConsequence().accept(this, data);
		System.out.println("consequence visit!");
		
		IASTexpression alternant = iast.getAlternant().accept(this, data);
		System.out.println("alternant visit!");
		//return iast.isTernary() ? f.newAlternative(condition,consequence,alternant().accpet(this,data))
		//: f.newAlternative(condition,consequence,null); 
		return f.newAlternative(condition,consequence,alternant);
	}

	@Override
	public IASTexpression visit(IASTbinaryOperation iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("binaryOperation visit!");
		return f.newBinaryOperation(iast.getOperator(), iast.getLeftOperand().accept(this, data),iast.getRightOperand().accept(this, data));
	}

	@Override
	public IASTexpression visit(IASTblock iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("block visit!");
		return f.newBlock(iast.getBindings(), iast.getBody().accept(this, data));
	}

	@Override
	public IASTexpression visit(IASTboolean iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("boolean visit!");
		return f.newBooleanConstant(iast.getDescription());
	}

	@Override
	public IASTexpression visit(IASTfloat iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("float visit!");
		return f.newFloatConstant(iast.getDescription());
	}

	@Override
	public IASTexpression visit(IASTinteger iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("integer visit!");
		return f.newIntegerConstant(iast.getDescription());
	}

	@Override
	public IASTexpression visit(IASTinvocation iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("invocation visit!");
		return f.newInvocation(iast.getFunction().accept(this, data), iast.getArguments());
	}

	@Override
	public IASTexpression visit(IASTsequence iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("sequence visit!");
		int length = iast.getExpressions().length;
    	IASTexpression[] exprs = new IASTexpression[length];
        for(int i = 0;i < length;i++) {
           	exprs[i] = iast.getExpressions()[i].accept(this, data);
        }
        return f.newSequence(exprs);
	}

	@Override
	public IASTexpression visit(IASTstring iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("string visit!");
		return f.newStringConstant(iast.getValue());
	}

	@Override
	public IASTexpression visit(IASTunaryOperation iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("unaryOperation visit!");
		int length = iast.getOperands().length;
    	IASTexpression[] exprs = new IASTexpression[length];
        for(int i = 0;i < length;i++) {
           	exprs[i] = iast.getOperands()[i].accept(this, data);
        }
        return f.newUnaryOperation(iast.getOperator(),iast.getOperand());
	}

	@Override
	public IASTexpression visit(IASTvariable iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("variable visit!");
		return f.newVariable(iast.getName());
	}

	@Override
	public IASTexpression visit(IASTunless iast, Void data) throws EvaluationException {
		// TODO Auto-generated method stub
		System.out.println("unless visit!");
		System.out.println("Method 2");
		IASTexpression condition = iast.getCondition().accept(this, data);
		System.out.println("conditionunless visit!");
		IASTexpression body = iast.getBody().accept(this, data);
		System.out.println("body visit!");
		return f.newAlternative(condition,null,body);
	}

	
}