import autopath
from pypy.tool import option
from optparse import make_option
from pypy.interpreter.pyparser import pyparse
from pypy.interpreter.astcompiler.astbuilder import ast_from_node
from pypy.interpreter.astcompiler import ast, consts

# Contexts: Load 1 | Store 2 | Del 3 | AugLoad 4 | AugStore 5 | Param 6

# Compare, BinOp, Return
def sexpy(node):
	if isinstance(node, ast.Module): 
		answer = ["'(Module"]
		for n in node.body:
			answer.append(sexpy(n))
		answer.append(")")
		return '\n'.join(answer)

	if isinstance(node, ast.BinOp):
		answer = ["(BinOp"]
		answer.append(sexpy(node.left))
		answer.append(sexpy(node.op))
		answer.append(sexpy(node.right))
		answer.append(")")
		return ' '.join(answer)

	if isinstance(node, ast.Return):
		answer = ["(Return"]
		answer.append(sexpy(node.value))
		answer.append(")")
		return ' '.join(answer)

	if isinstance(node, ast.Compare):
		answer = ["(Compare"]
		answer.append(sexpy(node.left))
		answer.append(sexpy(node.ops))
		answer.append(sexpy(node.comparators))
		answer.append(")")
		return ' '.join(answer)
		
	if isinstance(node, ast.Assign): 
		answer = ["(Assign"]
		answer.append(sexpy(node.targets))
		answer.append(sexpy(node.value))
		answer.append(")")
		return ' '.join(answer)

	if isinstance(node, ast.Call):
		answer = ["(Call "]
		answer.append(sexpy(node.func))
		answer.append(sexpy(node.args))
		answer.append(sexpy(node.starargs))
		answer.append(sexpy(node.kwargs))
		answer.append(")")
		return ' '.join(answer)

	if isinstance(node, ast.Name):
		return "(Name " + node.id + " " + str(node.ctx) + ")"

	if isinstance(node, ast.Num):
		return str(node.n.intval)

	if isinstance(node, ast.For):
		answer = ["(For"]
		answer.append(sexpy(node.target))
		answer.append(sexpy(node.iter))
		answer.append(sexpy(node.body))
		answer.append(sexpy(node.orelse))
		answer.append(")")
		return '\n'.join(answer)

	if isinstance(node, ast.FunctionDef):
		answer = ["(FunctionDef"]
		answer.append(sexpy(node.args))
		answer.append(sexpy(node.body))
		answer.append(sexpy(node.decorators))
		answer.append(")")
		return '\n'.join(answer)

	if isinstance(node, ast.Subscript):
		answer = ["(Subscript"]
		answer.append(sexpy(node.value))
		answer.append(sexpy(node.slice))
		answer.append(str(node.ctx))
		answer.append(")")
		return ' '.join(answer)

	if isinstance(node, ast.Ellipsis):
		return "(Ellipsis)"

	if isinstance(node, ast.Slice):
		return "(Slice " + sexpy(node.lower) + " " + sexpy(node.upper) + " " + sexpy(node.step) + ")"

	if isinstance(node, ast.ExtSlice):
		answer = ["(ExtSlice"]
		answer.append(sexpy(node.dims))
		answer.append(")")
		return ' '.join(answer)

	if isinstance(node, ast.Index):
		return "(Index " + sexpy(node.value) + ")"

	if isinstance(node, ast.arguments):
		answer = ["(arguments"]
		answer.append(sexpy(node.args))
		answer.append(sexpy(node.vararg))
		answer.append(sexpy(node.kwarg))
		answer.append(sexpy(node.defaults))
		answer.append(")")
		return ' '.join(answer)

	if isinstance(node, ast.If):
		answer = ["(If"]
		answer.append(sexpy(node.test))
		answer.append(sexpy(node.body))
		answer.append(sexpy(node.orelse))
		answer.append(")")
		return '\n'.join(answer)

	if isinstance(node, ast.Delete):
		answer = ["(Delete"]
		answer.append(sexpy(node.targets))
		answer.append(")")
		return ' '.join(answer)

	if isinstance(node, list):
		answer = ["("]
		for item in node:
			answer.append(sexpy(item))
		answer.append(")")
		return ' '.join(answer)

	if node == None:
		return "empty"
	
	return "'(" + node.__repr__() + ")"

targ_src = file("targ.py").read()

config, parser = option.get_standard_options()
spc = option.make_objspace(config)
p = pyparse.PythonParser(spc)

info = pyparse.CompileInfo("<test>", "exec")

tree = p.parse_source(targ_src, info)

ast_node = ast_from_node(spc, tree, info) # this is going to be an ast.Module object

print sexpy(ast_node)		
