package com.paracamplus.ilp2.ilp2tme6.parser.ilpml;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import antlr4.ILPMLgrammar2Lexer;
import antlr4.ILPMLgrammar2Parser;

import com.paracamplus.ilp2.ilp2tme6.transform.CopyTransform;
import com.paracamplus.ilp2.ilp2tme6.transform.RenameTransform;
import com.paracamplus.ilp2.interfaces.IASTfactory;
import com.paracamplus.ilp2.interfaces.IASTprogram;
import com.paracamplus.ilp2.parser.ilpml.ILPMLListener;
import com.paracamplus.ilp1.compiler.normalizer.NormalizationEnvironment;
import com.paracamplus.ilp1.parser.ParseException;

public class ILPMLOptimizingParser
extends com.paracamplus.ilp1.parser.ilpml.ILPMLParser {
	
	public ILPMLOptimizingParser(IASTfactory factory) {
		super(factory);
	}
		
	@Override
    public IASTprogram getProgram() throws ParseException {
		try {
			ANTLRInputStream in = new ANTLRInputStream(input.getText());
			// flux de caractères -> analyseur lexical
			ILPMLgrammar2Lexer lexer = new ILPMLgrammar2Lexer(in);
			// analyseur lexical -> flux de tokens
			CommonTokenStream tokens =	new CommonTokenStream(lexer);
			// flux tokens -> analyseur syntaxique
			ILPMLgrammar2Parser parser = new ILPMLgrammar2Parser(tokens);
			// démarage de l'analyse syntaxique
			ILPMLgrammar2Parser.ProgContext tree = parser.prog();		
			// parcours de l'arbre syntaxique et appels du Listener
			ParseTreeWalker walker = new ParseTreeWalker();
			ILPMLListener extractor1 = new ILPMLListener((IASTfactory)factory);
			walker.walk(extractor1, tree);
			CopyTransform copy = new CopyTransform((IASTfactory)factory);
			RenameTransform rename = new RenameTransform((IASTfactory)factory);
			copy.visit(tree.node,factory.newProgram(null));
			rename.visit(tree.node,NormalizationEnvironment.EMPTY);
			return tree.node;
		} catch (Exception e) {
			throw new ParseException(e);
		}
    }

}