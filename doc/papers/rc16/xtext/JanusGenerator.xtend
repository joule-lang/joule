package dk.sdu.mmmi.roo.generator

import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.emf.ecore.resource.Resource
import dk.sdu.mmmi.roo.rOO.Program
import java.util.Iterator
import java.util.Map
import org.eclipse.xtext.xbase.lib.Functions.Function2
import dk.sdu.mmmi.roo.rOO.ClassDecl
import java.util.Set
import dk.sdu.mmmi.roo.rOO.Procedure
import dk.sdu.mmmi.roo.rOO.Type
import dk.sdu.mmmi.roo.rOO.ConcreteProcedure
import org.eclipse.emf.common.util.EList
import dk.sdu.mmmi.roo.rOO.Local
import dk.sdu.mmmi.roo.rOO.Statement
import dk.sdu.mmmi.roo.rOO.Exp
import dk.sdu.mmmi.roo.rOO.Conditional
import dk.sdu.mmmi.roo.rOO.FromUntilLoop
import dk.sdu.mmmi.roo.rOO.ErrorStmt
import dk.sdu.mmmi.roo.rOO.Action
import dk.sdu.mmmi.roo.rOO.Body
import dk.sdu.mmmi.roo.rOO.Location
import dk.sdu.mmmi.roo.rOO.Invocation
import dk.sdu.mmmi.roo.rOO.Operation
import dk.sdu.mmmi.roo.rOO.OperationOP
import dk.sdu.mmmi.roo.rOO.NotDot
import dk.sdu.mmmi.roo.rOO.Constant
import dk.sdu.mmmi.roo.rOO.NewObject
import dk.sdu.mmmi.roo.rOO.Print
import dk.sdu.mmmi.roo.rOO.Constructor
import dk.sdu.mmmi.roo.rOO.NewArray
import dk.sdu.mmmi.roo.rOO.DelocalValue
import dk.sdu.mmmi.roo.rOO.DelocalArray
import dk.sdu.mmmi.roo.rOO.DelocalObject
import dk.sdu.mmmi.roo.rOO.DelocalNonObject
import java.util.HashSet
import dk.sdu.mmmi.roo.rOO.RefType
import dk.sdu.mmmi.roo.rOO.BaseType
import dk.sdu.mmmi.roo.rOO.DelocalAbstract
import dk.sdu.mmmi.roo.rOO.ExpOp

class JanusGenerator {
	
	static class Context {
		public ClassDecl cd;
		public Set<String> abstractParameters = new HashSet<String>
		public new(ClassDecl decl) { this.cd = decl; }
		def boolean isAbstractParameter(String name) { abstractParameters.contains(name) }
	}
	
	static class LHS {
		public String name;
		public String location;
		public boolean isAbstract;
		public new(String name, String lhs, Context context) { 
			this.name = name; this.location = lhs; this.isAbstract = context.isAbstractParameter(name)
		}
	}
	
	ROOAnalyzer generator
	Resource resource
	Program program
		
	new(ROOAnalyzer _generator, Resource _resource) {
		generator = _generator; resource = _resource;
		program = resource.allContents.filter(typeof(Program)).next
	}
	
	def <X,Y> String map2str(Map<X,Y> map, Function2<X,Y,String> fn) { 
		val buffer = new StringBuffer
		val data = map.entrySet
		for(d: data) buffer.append(fn.apply(d.key,d.value))
		buffer.toString
	}
	
	def doGenerate(IFileSystemAccess2 fsa) {
		fsa.generateFile(program.name+".ja",generateAll)
	}
	
	def generateAll() {
		val structure = generateHeader+generator.methodClassMap.map2str([x,y|generateDispatcher(x,y)])
		generator.classes(resource).fold(structure,[x,y|generateClass(x,y)])+generateMain
	}
	
	def generateHeader() '''
	// Generated program «program.name»
	// Memory characteristics: max object size = «generator.getMaxAllocSize», max stack depth = «program.depth»
	'''

	def generateMain() '''
	procedure main()
	  int heap[«program.depth»][«generator.getMaxAllocSize»]
	  int heap_counter = 1 // 0 represents a null value
	  «FOR loc:program.main.locals»
	  «loc.generateLocal(true,new Context(null))»
	  «ENDFOR»
	  «FOR s:program.main.statements»
	  «s.generateStatement(new Context(null))»
	  «ENDFOR»
	  «FOR loc:program.main.delocals»
	  «loc.generateDelocal(true,new Context(null))»
	  «ENDFOR»
	'''
	
	// DISPATCHERS

	def String generateDispatcher(Procedure proc, Set<ClassDecl> classes) '''
	procedure dispatch_«proc.generateDeclaration»
	  «IF classes.size>0»
	  «generateDispatcherHelper(proc,classes.iterator,classes.debuggingInfo)»
	  «ELSE»
	  error("No valid implementations")
	  «ENDIF»
	'''
	
	def String debuggingInfo(Set<ClassDecl> decls) {
		val result = new StringBuffer
		for(cd: decls) result.append(cd.name+"="+generator.classNumberingMap.get(cd.name)+" ")
		result.toString
	}
	
	def CharSequence generateDispatcherHelper(Procedure proc, Iterator<ClassDecl> cditerator, String debugInfo) '''
	«val className = cditerator.next.name»
	if heap[this][0]=«generator.classNumberingMap.get(className)» then
	  call «className»_«proc.id»(heap,heap_counter,this«IF proc.parameters.parameters.length>0»,«ENDIF»«FOR p:proc.parameters.parameters SEPARATOR ","»«p.id.generateVar»«ENDFOR»)
	else
	  «IF cditerator.hasNext»
	  «generateDispatcherHelper(proc,cditerator, debugInfo)»
	  «ELSE»
	  local int class_id = heap[this][0]
	  printf("Error when dispatching method «proc.id» to this=%d, class=%d, «debugInfo»",this,class_id)
	  delocal int class_id = heap[this][0]
	  error("Method not found")
	  «ENDIF»
	fi heap[this][0]=«generator.classNumberingMap.get(className)»
	'''
	
	// INFRASTRUCTURE

	def String generateDeclaration(Procedure proc) '''«proc.id»(int heap[][],int heap_counter,int this«IF proc.parameters.parameters.length>0»,«ENDIF»«FOR p:proc.parameters.parameters SEPARATOR ","»«p.type.generateTypeDecl(p.id)»«ENDFOR»)'''

	def generateTypeDecl(Type t, String var_id) { "int "+var_id.generateVar }
	
	val MAGIC_ESCAPE="%"
	
	def generateVar(String name) { 
		if(name.equals("this")) name 
		else if(name.equals("null")) "0"
		else if(name.startsWith(MAGIC_ESCAPE)) name.substring(MAGIC_ESCAPE.length)
		else "var_"+name
	}
		
	// CLASSES
		
	def String generateClass(String done, ClassDecl cd) {
		val result = new StringBuffer(done)
		if(cd.constructor==null) 
			result.append(cd.generateDefaultConstructor)
		else
			result.append(cd.constructor.generateConstructor(cd))
		//if(cd.destructor!=null) result.append(cd.destructor.generateDestructor(cd))
		for(p: cd.procedures) {
			switch p {
				ConcreteProcedure: {
					result.append(p.generateProcedure(cd))
				}
			}
		}
		result.toString
	}
	
	def generateConstructor(Constructor constructor, ClassDecl cd) '''
	procedure constructor_«cd.name»(int heap[][],int heap_counter,int this«IF constructor.parameters.parameters.length>0»,«ENDIF»«FOR p:constructor.parameters.parameters SEPARATOR ","»«p.type.generateTypeDecl(p.id)»«ENDFOR»)
	  local int this_check = 0
	  «cd.constructorHeader»
	  «constructor.body.generateBody(new Context(cd))»
	  this_check += heap[this][0]
	  delocal int this_check = «generator.classNumberingMap.get(cd.name)»
	'''
	
	def generateDefaultConstructor(ClassDecl cd) '''
	procedure constructor_«cd.name»(int heap[][],int heap_counter,int this)
	  «cd.constructorHeader»
	'''
		
	def constructorHeader(ClassDecl cd) '''heap[this][0] += «generator.classNumberingMap.get(cd.name)»'''
	
	def destructorFooter(ClassDecl cd) '''heap[this][0] -= «generator.classNumberingMap.get(cd.name)»'''
	
	def generateProcedure(ConcreteProcedure proc, ClassDecl cd) '''
	procedure «cd.name»_«proc.generateDeclaration»
	  local int heap_counter_check = heap_counter
	  local int this_check = this
	  local int this_class_check = heap[this][0]
	  «proc.body.generateBody(proc.contextInfo(cd))»
	  delocal int this_class_check = heap[this][0]
	  delocal int this_check = this
	  delocal int heap_counter_check = heap_counter
	'''
	
	def Context contextInfo(ConcreteProcedure procedure, ClassDecl decl) {
		val context = new Context(decl)
		val aps = context.abstractParameters
		for(p: procedure.parameters.parameters) {
			val t = p.type
			switch t {
				RefType: if(t.isAbstract) aps.add(p.id)
			}
		}
		return context
	}
	
	def CharSequence generateBody(Body body, Context context) '''
	  «FOR loc:body.locals»
	  «loc.generateLocal(false,context)»
	  «ENDFOR»
	  «FOR s:body.statements»
	  «s.generateStatement(context)»
	  «ENDFOR»
	  «FOR dloc:body.delocals»
	  «dloc.generateDelocal(false,context)»
	  «ENDFOR»
	'''
	
	def generateLocal(Local loc, boolean isMain, Context context) { 
		val base = (if(!isMain) "local " else "")+loc.type.generateTypeDecl(loc.id)
		if(loc.init==null) {
			if(loc.type.isAbstractDecl) {
				addPostLine(loc.id.generateVar+" += heap_counter")
				addPostLine("heap_counter += 1 // abstract object preallocation")
				return base+" = 0"
			} else {
				return base
			}
		} else {
			return base+" = "+loc.init.generateExp(new LHS(loc.id,loc.id.generateVar,context),context)
		}
	}
	
	def dispatch boolean isAbstractDecl(BaseType type) { return false }
	def dispatch boolean isAbstractDecl(RefType type) { return type.isAbstract }
	
	def dispatch generateDelocal(DelocalObject dloco, boolean isMain, Context context) '''
	uncall constructor_«dloco.classdecl.name»«dloco.arguments.generateArguments(dloco.id,context)»
	heap_counter -= 1
	«IF isMain»
	«dloco.id.generateVar» -= heap_counter
	if «dloco.id.generateVar» != 0 then
	  error("Reference not zeroed after object deallocation")
	fi «dloco.id.generateVar» != 0
	«ELSE»
	delocal int «dloco.id.generateVar» = heap_counter
	«ENDIF»
	'''
		
	def dispatch generateDelocal(DelocalNonObject dloc, boolean isMain, Context context) {
		val spec = dloc.spec
		switch spec {
			DelocalValue: (if(isMain) dloc.id.generateVar+"-=" else "delocal int "+dloc.id.generateVar+" = ")+spec.value.generateExp(new LHS(dloc.id,dloc.id.generateVar,context),context) 
			DelocalArray: throw new Error("not implemented yet")
		}	
	}
	
	def dispatch generateDelocal(DelocalAbstract dloca, boolean isMain, Context context) '''
	heap_counter -= 1
	«IF isMain»
	«dloca.id.generateVar» -= heap_counter
	«ELSE»
	delocal int «dloca.id.generateVar» = heap_counter
	«ENDIF»
	'''
	
	// STATEMENTS
	var statementFollowingLineBuffer = new StringBuffer()
	var statementBeforeDelocalBuffer = new StringBuffer()
	
	def void addPostLine(String line) { statementFollowingLineBuffer.append(line+"\n") }
	def void addPreDelocalLine(String line) { statementBeforeDelocalBuffer.append(line+"\n") }
	
	def CharSequence generateStatement(Statement stmt, Context context) {
		var pretext = ""
		if(statementFollowingLineBuffer.length>0) {
			pretext = statementFollowingLineBuffer.toString
			statementFollowingLineBuffer = new StringBuffer
		}
		var CharSequence result = stmt.generateStatementImpl(context)
		result = pretext+result
		if(statementFollowingLineBuffer.length>0) {
			result = result+"\n"+statementFollowingLineBuffer.toString
			statementFollowingLineBuffer = new StringBuffer
		}
		result
	}
	
	// Conditional | FromUntilLoop | ErrorStmt | DeAlloc | Action	
	def dispatch CharSequence generateStatementImpl(Conditional con, Context context) '''
	if «con.pre.generateExp(null,context)» then
	  «con.thenbody.generateBody(context)»
	else
	  «con.elsebody.generateBody(context)»
	fi «con.post.generateExp(null,context)»
	'''
	def dispatch CharSequence generateStatementImpl(FromUntilLoop ful, Context context) '''
	from «ful.from.generateExp(null,context)» loop
	  «ful.body.generateBody(context)»
	until «ful.until.generateExp(null,context)»
	'''

	def dispatch CharSequence generateStatementImpl(ErrorStmt err, Context context) '''
	error("«err.msg»")
	'''

	def dispatch CharSequence generateStatementImpl(Print print, Context context) '''
	printf("«print.msg»\\n"«IF print.value!=null»,«print.value.generateVar»«ENDIF»)
	'''
	
	def dispatch CharSequence generateStatementImpl(Action act, Context context) { 
		act.rest.generateActionRest(act.lhs,context)
	}
	
	def generateOperationLocation(Location loc, Context context) {
		if(loc.field==null && loc.index==null)
			return loc.id.generateVar
		else if(loc.index==null)
			return "heap["+loc.id.generateVar+"]["+generator.getFieldIndex(context.cd,loc.field)+"]"
		else if(loc.field==null)
			return "heap["+loc.id.generateVar+"]["+generator.ARRAY_HEADER_SIZE+"+"+loc.index.generateExp(null,context)+"]"
		else
			return "heap[heap["+loc.id.generateVar+"]["+generator.getFieldIndex(context.cd,loc.field)+"]]["+generator.ARRAY_HEADER_SIZE+"+"+loc.index.generateExp(null,context)+"]"
	}

	def dispatch generateActionRest(Invocation inv, Location loc, Context context) {
		if(loc.field==null || loc.index!=null) throw new Error("Illegal invocation target")
		val un = switch loc.dot { NotDot: "un" default: "" }
		return un+"call dispatch_"+loc.field+inv.arguments.generateArguments(loc.id,context)
	}
	
	def dispatch generateActionRest(Operation ope, Location loc, Context context) '''
	«loc.generateOperationLocation(context)» «ope.op.generateOperator» «ope.rhs.generateExpFragment(new LHS(loc.id,loc.generateOperationLocation(context),context),context)»
	'''

	def dispatch CharSequence generateExpFragment(Location loc, LHS lhs, Context context) { 
		loc.generateOperationLocation(context)
	}

	def dispatch CharSequence generateExpFragment(Constant con, LHS lhs, Context context) { 
		Integer.toString(con.value)
	}

	def dispatch CharSequence generateExpFragment(NewObject neo, LHS lhs, Context context) {
		if(lhs==null) throw new Error("Cannot generate object instantation without legal left-hand side")
		if(lhs.isAbstract) {
			addPostLine("// Abstract parameter object initialization")
			addPostLine("call constructor_"+neo.classdecl.name+neo.arguments.generateArguments(MAGIC_ESCAPE+lhs.location,context))
		} else {
			addPostLine(lhs.location+" += heap_counter")
			addPostLine("heap_counter += 1 // object pseudo allocation")
			addPostLine("call constructor_"+neo.classdecl.name+neo.arguments.generateArguments(MAGIC_ESCAPE+lhs.location,context))
		}
		return "0"
	}

	def dispatch CharSequence generateExpFragment(NewArray nea, LHS lhs, Context context) {
		if(lhs==null) throw new Error("Cannot generate array instantation without legal left-hand side")
		addPostLine(lhs.location+" += heap_counter")
		addPostLine("heap_counter += 1 // array pseudo allocation")
		"0"
	}

	def CharSequence generateArguments(EList<Exp> args, String target, Context context) '''(heap,heap_counter«IF target!=null»,«target.generateVar»«ENDIF»«IF args.length>0»,«ENDIF»«FOR e:args SEPARATOR ","»«e.generateExp(null,context)»«ENDFOR»)'''

	def String generateOperator(OperationOP op) { if(op.t.equals(":=")) "+=" else op.t }
	
	def String generateOperator(ExpOp op) { if(op.t.equals("==")) "=" else op.t }

	// EXPRESSIONS
	
	def CharSequence generateExp(Exp exp, LHS lhs, Context context) { 
		val result = exp.prim.generateExpFragment(lhs,context)
		return if(exp.op==null) result else result+exp.op.generateOperator+exp.more.generateExp(lhs,context)
	}
	
	
}