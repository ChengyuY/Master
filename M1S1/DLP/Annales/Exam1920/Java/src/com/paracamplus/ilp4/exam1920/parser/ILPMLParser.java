package com.paracamplus.ilp4.exam1920.parser;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import antlr4.ILPMLgrammar4exam1920Lexer;
import antlr4.ILPMLgrammar4exam1920Parser;

import com.paracamplus.ilp4.exam1920.interfaces.IASTfactory;
import com.paracamplus.ilp4.interfaces.IASTprogram;
import com.paracamplus.ilp1.parser.ParseException;

public class ILPMLParser
extends com.paracamplus.ilp4.parser.ilpml.ILPMLParser {
	
	public ILPMLParser(IASTfactory factory) {
		super(factory);
	}
		
	@Override
    public IASTprogram getProgram() throws ParseException {
		try {
			ANTLRInputStream in = new ANTLRInputStream(input.getText());
			ILPMLgrammar4exam1920Lexer lexer = new ILPMLgrammar4exam1920Lexer(in);
			CommonTokenStream tokens =	new CommonTokenStream(lexer);
			ILPMLgrammar4exam1920Parser parser = new ILPMLgrammar4exam1920Parser(tokens);
			ILPMLgrammar4exam1920Parser.ProgContext tree = parser.prog();		
			ParseTreeWalker walker = new ParseTreeWalker();
			ILPMLListener extractor = new ILPMLListener((IASTfactory)factory);
			walker.walk(extractor, tree);	
			return tree.node;
		} catch (Exception e) {
			throw new ParseException(e);
		}
    }

}
