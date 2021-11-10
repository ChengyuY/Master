package com.paracamplus.ilp4.exam1920.ast;

import com.paracamplus.ilp1.interfaces.IASTalternative;
import com.paracamplus.ilp1.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp1.interfaces.IASTblock;
import com.paracamplus.ilp1.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp1.interfaces.IASTboolean;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp1.interfaces.IASTfloat;
import com.paracamplus.ilp1.interfaces.IASTinteger;
import com.paracamplus.ilp1.interfaces.IASTinvocation;
import com.paracamplus.ilp1.interfaces.IASToperator;
import com.paracamplus.ilp1.interfaces.IASTsequence;
import com.paracamplus.ilp1.interfaces.IASTstring;
import com.paracamplus.ilp1.interfaces.IASTunaryOperation;
import com.paracamplus.ilp1.interfaces.IASTvariable;
import com.paracamplus.ilp1.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp2.interfaces.IASTassignment;
import com.paracamplus.ilp2.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp2.interfaces.IASTloop;
import com.paracamplus.ilp2.interfaces.IASTprogram;
import com.paracamplus.ilp3.interfaces.IASTcodefinitions;
import com.paracamplus.ilp3.interfaces.IASTlambda;
import com.paracamplus.ilp3.interfaces.IASTnamedLambda;
import com.paracamplus.ilp3.interfaces.IASTtry;
import com.paracamplus.ilp4.exam1920.interfaces.IASTforeach;
import com.paracamplus.ilp4.exam1920.interfaces.IASTvisitor;
import com.paracamplus.ilp4.interfaces.IASTclassDefinition;
import com.paracamplus.ilp4.interfaces.IASTfieldRead;
import com.paracamplus.ilp4.interfaces.IASTfieldWrite;
import com.paracamplus.ilp4.interfaces.IASTinstantiation;
import com.paracamplus.ilp4.interfaces.IASTself;
import com.paracamplus.ilp4.interfaces.IASTsend;
import com.paracamplus.ilp4.interfaces.IASTsuper;

public class ForeachTransform<Data>
implements IASTvisitor<IASTexpression, Data, EvaluationException> {

	protected ASTfactory factory;
		
	public ForeachTransform(ASTfactory factory) {
		this.factory = factory;
	}
	
	// Copie

	@Override
	public IASTexpression visit(IASTassignment iast, Data data)
			throws EvaluationException {
		IASTvariable variable = (IASTvariable) iast.getVariable().accept(this,data);
		IASTexpression value = iast.getExpression().accept(this, data);
		return factory.newAssignment(variable, value);
	}

	@Override
	public IASTexpression visit(IASTloop iast, Data data)
			throws EvaluationException {
		IASTexpression condition = iast.getCondition().accept(this,data);
		IASTexpression body = iast.getBody().accept(this,data);
		return factory.newLoop(condition, body);
	}

	@Override
	public IASTexpression visit(IASTalternative iast, Data data)
			throws EvaluationException {
		IASTexpression condition = iast.getCondition().accept(this, data);
		IASTexpression consequence = iast.getConsequence().accept(this,data);
		IASTexpression alternant = iast.getAlternant();
		if (alternant != null) {
			alternant = alternant.accept(this,data); 
		}
		return factory.newAlternative(condition, consequence, alternant);
	}

	@Override
	public IASTexpression visit(IASTbinaryOperation iast, Data data)
			throws EvaluationException {
		IASTexpression leftOperand = iast.getLeftOperand().accept(this,data);
		IASTexpression rightOperand = iast.getRightOperand().accept(this,data);
		IASToperator operator = iast.getOperator();
		return factory.newBinaryOperation(operator, leftOperand, rightOperand);
	}

	@Override
	public IASTexpression visit(IASTblock iast, Data data)
			throws EvaluationException {
		IASTbinding[] oldbinding = iast.getBindings();
		IASTbinding[] binding =new IASTbinding[oldbinding.length];
		IASTexpression body = iast.getBody().accept(this,data);
		for (int i = 0; i < oldbinding.length; i++) {
			IASTvariable v = (IASTvariable) oldbinding[i].getVariable().accept(this,data);
			IASTexpression exp = oldbinding[i].getInitialisation().accept(this, data);
			binding[i] = factory.newBinding(v, exp);
		}
		return factory.newBlock(binding, body);
	}

	@Override
	public IASTexpression visit(IASTboolean iast, Data data)
			throws EvaluationException {
		return factory.newBooleanConstant(iast.getValue().toString());
	}

	@Override
	public IASTexpression visit(IASTfloat iast, Data data)
			throws EvaluationException {
		return factory.newFloatConstant(iast.getValue().toString());
	}

	@Override
	public IASTexpression visit(IASTinteger iast, Data data)
			throws EvaluationException {
		return factory.newIntegerConstant(iast.getValue().toString());
	}

	@Override
	public IASTexpression visit(IASTinvocation iast, Data data)
			throws EvaluationException {
		IASTexpression function = iast.getFunction().accept(this, data);
		IASTexpression[] oldarguments = iast.getArguments();
		IASTexpression[] arguments = new IASTexpression[oldarguments.length];
		for (int i = 0; i< oldarguments.length; i++) {
			arguments[i] = oldarguments[i].accept(this, data);
		}
		return factory.newInvocation(function, arguments);
	}

	@Override
	public IASTexpression visit(IASTsequence iast, Data data)
			throws EvaluationException {
		IASTexpression[] oldasts = iast.getExpressions();
		IASTexpression[] asts = new IASTexpression[oldasts.length];
		for (int i = 0; i < oldasts.length; i++) {
			asts[i] = oldasts[i].accept(this, data);
		}
		return factory.newSequence(asts);
	}

	@Override
	public IASTexpression visit(IASTstring iast, Data data)
			throws EvaluationException {
		return factory.newStringConstant(iast.getValue());
	}

	@Override
	public IASTexpression visit(IASTunaryOperation iast, Data data)
			throws EvaluationException {
		IASToperator operator = iast.getOperator();
		IASTexpression operand = iast.getOperand().accept(this,data);
		return factory.newUnaryOperation(operator, operand);
	}

	@Override
	public IASTexpression visit(IASTvariable iast, Data data)
			throws EvaluationException {
		return factory.newVariable(iast.getName());
	}
	
	
	public IASTfunctionDefinition visit(IASTfunctionDefinition iast, Data data) 
			throws EvaluationException {
		IASTvariable functionVariable  = (IASTvariable) iast.getFunctionVariable().accept(this,data);
		IASTvariable[] oldvariables = iast.getVariables();
		IASTvariable[] variables = new IASTvariable[oldvariables.length];
		IASTexpression body = iast.getBody().accept(this, data);
		for (int i = 0; i < oldvariables.length; i++) {
			variables[i] = (IASTvariable) oldvariables[i].accept(this,data);
		}
		return factory.newFunctionDefinition(functionVariable, variables, body);
	}
	
	public IASTprogram visit(IASTprogram iast, Data data) 
			throws EvaluationException {
		IASTexpression expression = iast.getBody().accept(this, data);
		IASTfunctionDefinition[] oldfunctions = iast.getFunctionDefinitions();
		IASTfunctionDefinition[] functions = new IASTfunctionDefinition[oldfunctions.length];
		for (int i = 0; i < oldfunctions.length; i++) {
			functions[i] = visit(oldfunctions[i], data);
		}
		// TODO: also transform inside methods...
		IASTclassDefinition[] clazz = ((com.paracamplus.ilp4.interfaces.IASTprogram)iast).getClassDefinitions();
		return factory.newProgram(functions, clazz, expression);
	}

	@Override
	public IASTexpression visit(IASTinstantiation iast, Data data) throws EvaluationException {
		IASTexpression[] oldasts = iast.getArguments();
		IASTexpression[] asts = new IASTexpression[oldasts.length];
		for (int i = 0; i < oldasts.length; i++) {
			asts[i] = oldasts[i].accept(this, data);
		}
		return factory.newInstantiation(iast.getClassName(), asts);
	}

	@Override
	public IASTexpression visit(IASTfieldRead iast, Data data) throws EvaluationException {
		return factory.newReadField(iast.getFieldName(), iast.getTarget().accept(this,  data));
	}

	@Override
	public IASTexpression visit(IASTself iast, Data data) throws EvaluationException {
		return iast;
	}

	@Override
	public IASTexpression visit(IASTsend iast, Data data) throws EvaluationException {
		IASTexpression[] oldasts = iast.getArguments();
		IASTexpression[] asts = new IASTexpression[oldasts.length];
		for (int i = 0; i < oldasts.length; i++) {
			asts[i] = oldasts[i].accept(this, data);
		}
		return factory.newSend(iast.getMethodName(), iast.getReceiver().accept(this, data), asts);
	}

	@Override
	public IASTexpression visit(IASTsuper iast, Data data) throws EvaluationException {
		return iast;
	}

	@Override
	public IASTexpression visit(IASTfieldWrite iast, Data data) throws EvaluationException {
		return factory.newWriteField(iast.getFieldName(), iast.getTarget().accept(this,  data), iast.getValue().accept(this, data));
	}

	@Override
	public IASTexpression visit(IASTcodefinitions iast, Data data) throws EvaluationException {
	    IASTnamedLambda[] fn = iast.getFunctions();
	    IASTnamedLambda[] fn2 = new IASTnamedLambda[fn.length];
	    for (int i = 0; i < fn.length; i++) {
	    	fn2[i] = factory.newNamedLambda(fn[i].getFunctionVariable(), fn[i].getVariables(), fn[i].getBody().accept(this,  data));
	    }
		return factory.newCodefinitions(fn2,  iast.getBody().accept(this,  data));
	}

	@Override
	public IASTexpression visit(IASTlambda iast, Data data) throws EvaluationException {
		return factory.newLambda(iast.getVariables(),  iast.getBody().accept(this,  data));
	}

	@Override
	public IASTexpression visit(IASTtry iast, Data data) throws EvaluationException {
		return factory.newTry(
				iast.getBody().accept(this, data),
				iast.getCatcher() == null ? null : (IASTlambda)iast.getCatcher().accept(this,  data),
				iast.getFinallyer() == null ? null : iast.getFinallyer().accept(this,  data)
		);
	}

	
	// MODIF EXAM

	@Override
	public IASTexpression visit(IASTforeach iast, Data data) throws EvaluationException {
		// get iterable
		IASTexpression e1 = iast.getIterator().accept(this, data);
		// get iterator
		IASTvariable iterator = factory.newVariable("$iterator");
		IASTexpression e2 = factory.newSend("iterator", e1, new IASTexpression[0]);
		// get value
		IASTvariable variable = iast.getVariable();
		IASTexpression e3 = factory.newSend("next", iterator, new IASTexpression[0]);
		// loop
		IASTexpression body = 
				factory.newSequence(new IASTexpression[] {
						iast.getBody().accept(this, data),
						factory.newAssignment(variable, e3)
				});
		IASTexpression loop = factory.newLoop(variable, body);
		// blocks
		IASTblock.IASTbinding b1 = factory.newBinding(variable, e3);
		IASTexpression block1 = factory.newBlock(new IASTbinding[] { b1 }, loop);
		IASTblock.IASTbinding b2 = factory.newBinding(iterator, e2);
		IASTexpression block2 = factory.newBlock(new IASTbinding[] { b2 }, block1);
		return block2;
	}

}
