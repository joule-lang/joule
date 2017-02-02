/*
Copyright (c) 2017, Ulrik Pagh Schultz, University of Southern Denmark
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies, 
either expressed or implied, of the University of Southern Denmark.
*/

package dk.sdu.mmmi.roo.generator

import org.eclipse.emf.ecore.resource.Resource
import dk.sdu.mmmi.roo.rOO.ClassDecl
import java.util.HashMap
import dk.sdu.mmmi.roo.rOO.NewObject
import org.eclipse.xtend.lib.annotations.Accessors
import java.util.Map
import java.util.Set
import dk.sdu.mmmi.roo.rOO.Procedure
import java.util.HashSet
import dk.sdu.mmmi.roo.rOO.Program

class ROOAnalyzer {
	
	public val OBJECT_HEADER_SIZE = 1
	public val ARRAY_HEADER_SIZE = 1

	@Accessors(PUBLIC_GETTER) val Map<String,ClassDecl> className = new HashMap<String,ClassDecl>
	@Accessors(PUBLIC_GETTER) val Map<String,Integer> instanceSize = new HashMap<String,Integer>
	@Accessors(PUBLIC_GETTER) var int maxAllocSize = 1 // Minimum array size allowed by Janus
	@Accessors(PUBLIC_GETTER) val Map<Procedure,Set<ClassDecl>> methodClassMap = new HashMap<Procedure,Set<ClassDecl>>
	val methodResolutionMap = new HashMap<String,Procedure>
	@Accessors(PUBLIC_GETTER) val Map<String,Integer> classNumberingMap = new HashMap<String,Integer>
	var classNumberCounter = 1
	@Accessors(PUBLIC_GETTER) val Map<String,Map<String,Integer>> fieldLocationMap = new HashMap<String,Map<String,Integer>>

	def void analyze(Resource resource) {
		resource.classes.forEach[className.put(it.name,it)]
		resource.classes.forEach[classNumberingMap.put(it.name,classNumberCounter++)]
		resource.classes.forEach[computeClassSize]
		resource.classes.forEach[computeFieldIndices]
		val program = resource.allContents.filter(typeof(Program)).next
		maxAllocSize = Math.max(maxAllocSize,program.width)
		resource.allContents.filter(typeof(NewObject)).forEach[maxAllocSize=Math.max(Math.max(maxAllocSize,it.objectSize),it.maxArraySize)]
		resource.classes.forEach[computeMethodMap(methodResolutionMap)]
	}
	
	def classes(Resource resource) { resource.allContents.filter(typeof(ClassDecl))	}
	
	def int objectSize(NewObject object) { instanceSize.get(object.classdecl.name) }
	
	def int maxArraySize(NewObject object) {
		ARRAY_HEADER_SIZE+object.dimensions.fold(0,[x,y|Math.max(x,y)])
	}
	
	def int computeClassSize(ClassDecl cd) {
		if(instanceSize.get(cd.name)!=null) return instanceSize.get(cd.name)
		val zuper = if(cd.zuper==null) OBJECT_HEADER_SIZE else cd.zuper.computeClassSize
		val total = cd.attributes.length+zuper
		instanceSize.put(cd.name, total)
		return total
	}
	
	def void computeMethodMap(ClassDecl cd, HashMap <String,Procedure> methodNameMap) {
		for(p: cd.procedures) {
			// Unique procedure as key, currently per-name and ignoring class hierarchy etc
			var proc = p
			if(!methodNameMap.containsKey(proc.id)) {
				methodNameMap.put(proc.id,proc)
			} else proc = methodNameMap.get(proc.id)
			// Associate class to procedure key
			var s = methodClassMap.get(proc)
			if(s==null) {
				s = new HashSet<ClassDecl>
				methodClassMap.put(proc,s)
			}
			s.add(cd)
		}
	}
	
	def void computeFieldIndices(ClassDecl cd) {
		val localOffset = instanceSize.get(cd.name)-cd.attributes.length
		val Map<String,Integer> fieldMap = new HashMap<String,Integer>
		fieldLocationMap.put(cd.name,fieldMap)
		var offset = 0
		for(attr: cd.attributes) {
			fieldMap.put(attr.name, localOffset+(offset++))
		}
	}
	
	def getFieldIndex(ClassDecl decl, String fieldName) {
		val result = fieldLocationMap.get(decl.name).get(fieldName)
		if(result==null) throw new Error("Field lookup failed: "+decl.name+"."+fieldName) else result
	}
		
}