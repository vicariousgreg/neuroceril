from gnetwork import *
from time import perf_counter

def gen_gh_mappings(gate_sequences, gh):
    maps = []

    # Add gate sequences
    sym_gates = {}
    for start_key,term_key,seq in gate_sequences:
        names = [("%s_%02d" % (start_key,i)) for i,gs in enumerate(seq)]
        names[0] = start_key

        # add in gh<-gh
        for name, gs in zip(names, seq):
            sym_gates[name] = (gs
                if any(g[:2] == (gh, "activate") for g in gs)
                else gs + ((gh, "activate", gh, "hetero"),))

        for prev,nxt in zip(names, names[1:]):
            maps.append((nxt, prev))

        if term_key:
            maps.append((term_key, names[-1]))

    sym_gates.update({"halt" : ()})
    return { (gh, gh, "hetero") : maps }, sym_gates

def gen_op_mappings(operations, op_layer, lex_layer, gh_layer):
    mappings = {
        (op_layer, op_layer, "hetero") : [],
        (op_layer, lex_layer, "hetero") : [],
        (gh_layer, op_layer, "hetero") : [],
        (lex_layer, op_layer, "hetero") : [],
    }

    for start_key,term_key,ops in operations:
        # Add None for missing args
        ops = [
            op if len(op) == 2 else op+(None,)
            for op in ops
        ]

        # Create name mapping (for op_exec)
        mappings[op_layer, lex_layer, "hetero"].append(
            (start_key, start_key))

        names = [
            "%s_%02d" % (start_key,i)
            for i,op in enumerate(ops)]
        names[0] = start_key

        for name,(gh,var) in zip(names, ops):
            mappings[gh_layer, op_layer, "hetero"].append(
                (gh, name))

            if var:
                mappings[lex_layer, op_layer, "hetero"].append(
                    (var, name))

        if term_key:
            names.append(term_key)

        for prev,nxt in zip(names, names[1:]):
            mappings[op_layer, op_layer, "hetero"].append(
                (nxt, prev))

    return mappings

def gen_stack_mappings(stack_layer, size, auto=False):
    mappings = {
        (stack_layer, stack_layer, "fwd") : [],
        (stack_layer, stack_layer, "bwd") : [],
    }
    if auto:
        mappings[stack_layer, stack_layer, "auto"] = []

    for i in range(size):
        curr = str(i)
        successor = str((i+1) % size)

        mappings[stack_layer, stack_layer, "fwd"].append((successor, curr))
        mappings[stack_layer, stack_layer, "bwd"].append((curr, successor))
        if auto:
            mappings[stack_layer, stack_layer, "auto"].append((curr, curr))

    return mappings

def gen_mem_mappings(prog, mem_layer, mem_ctx_layer, lex_layer, pre_mem_syms=set()):
    mappings = {
        (mem_ctx_layer, mem_layer, "hetero") : [],
        (mem_layer, mem_layer, "hetero", mem_ctx_layer) : [],
        (mem_layer, mem_layer, "auto") : [],
        (lex_layer, mem_layer, "hetero") : [],
        (mem_layer, lex_layer, "hetero") : [],

        #(mem_layer, mem_layer, "auto", mem_ctx_layer) : [],
    }

    states = []
    links = []
    symbols = set().union(pre_mem_syms)

    existing = { () : "NIL" }

    def compil(exp):
        if type(exp) is tuple:
            if exp in existing:
                # Recycle expression
                return existing[exp]
            else:
                curr = "main%d" % len(states)
                states.append((curr, "#LIST"))
                links.append((curr, compil(exp[0]), compil(exp[1:])))
                existing[exp] = curr
                return curr
        else:
            symbols.add(exp)
            return exp

    if len(prog) > 0:
        compil(prog)

    # recycle shared symbols
    for sym in symbols:
        states.append((sym, sym))
        mappings[mem_layer, lex_layer, "hetero"].append((sym, sym))

    for state,sym in states:
        mappings[mem_ctx_layer, mem_layer, "hetero"].append((state, state))
        mappings[mem_layer, mem_layer, "auto"].append((state, state))
        mappings[lex_layer, mem_layer, "hetero"].append((sym, state))

    for parent,first,rest in links:
        mappings[mem_layer, mem_layer, "hetero", mem_ctx_layer].append((
            first, parent, parent))
        mappings[mem_layer, mem_layer, "hetero", mem_ctx_layer].append((
            rest, first, parent))
        #mappings[mem_layer, mem_layer, "auto", mem_ctx_layer].extend((
        #    (first, first, parent),
        #    (rest, rest, parent)))
        #mappings[mem_layer, mem_layer, "auto"].extend((
        #    (first, first),
        #    (rest, rest)))

    # Empty list car/cdr is empty list
    mappings[mem_layer, mem_layer, "hetero", mem_ctx_layer].append((
        "NIL", "NIL", "NIL"))

    return mappings


def flash_mappings(gate, mappings, network, count=3, verbose=False):
    if len(mappings):
        if len(gate) == 3:
            to_layer, from_layer, name = gate
            to_sym, from_sym = zip(*mappings)
            ctx_layer = None
            ctx_sym = []
        elif len(gate) == 4:
            to_layer, from_layer, name, ctx_layer = gate
            to_sym, from_sym, ctx_sym = zip(*mappings)
            ctx_layer = network.layers[ctx_layer]
        # To handle different source/target contexts,
        #     add keys of length 5 (append source context)

        # Do not decay with flashed mappings
        network.get_connection(to_layer, from_layer, name).flash(
            to_sym, from_sym,
            ctx_layer=ctx_layer, ctx_syms=ctx_sym,
            from_mask = True, to_mask = True,
            learning_rule="rehebbian",
            decay=1.0,
            count=count,
            verbose=verbose)

def gen_machine(prog="", verbose=False, layer_sizes={}, capacity={}, ctx_lam = {},
        ortho=True, emulate=False, check=False, decay=1.0):
    operations = [
        ("halt", "halt",
         (("halt",),)),

        ("init", "halt",
         (("init_exc_stack",),
          ("op_exec","eval_main"),)),

        ("init_repl", "repl",
         (("init_exc_stack",),)),

        ("return", None,
         (("adv_stack_prev",),
          ("get_stack_op",))),

        ("pass", None,
         (("adv_stack_prev",),
          ("get_stack_op",))),

        ("print_halt", "halt",
         (("print_mem",),
          ("halt",),)),

        ("error", "halt",
         (("print_sym","ERROR"),
          ("op_exec", "print"),)),

        ("crash_lookup", "print_halt",
         (("print_sym","LOOKUP-ERROR"),)),

        ("error_lookup", "return",
         (("op_branch_exc_stack", "crash_lookup"),
          ("pop_exc_stack",),)),

        ("crash_object", "print_halt",
         (("print_sym","OBJECT-ERROR"),)),

        ("error_object", "return",
         (("op_branch_exc_stack", "crash_object"),
          ("pop_exc_stack",),)),

        ("error_exec", "print_halt",
         (("print_sym","APPLICATION-ERROR"),)),

        ("NIL", "print_halt",
         (("print_sym","APPLICATION-ERROR"),
          ("car_mem",),)),

        ("true", "print_halt",
         (("print_sym","APPLICATION-ERROR"),
          ("car_mem",),)),

        ("false", "print_halt",
         (("print_sym","APPLICATION-ERROR"),
          ("car_mem",),)),

        ("false_or_nil?", "get_false_mem",
         (("cmp_lex", "false"),
          ("op_branch_mem_lex","get_true_mem"),
          ("cmp_lex", "NIL"),
          ("op_branch_mem_lex","get_true_mem"),
         )),

        ("get_true_mem", "return",
         (("get_sym_mem", "true"),)),

        ("get_false_mem", "return",
         (("get_sym_mem", "false"),)),

        ("get_nil_mem", "return",
         (("get_sym_mem", "NIL"),)),

        ########################################################################
        # Core interpreter procedures
        ########################################################################

        ("push_args", "push_args",
         (("cmp_lex","NIL"),
          ("op_branch_mem_lex","return"),

          ("bind_mem_stack",),

          ("car_mem",),
          ("op_exec","eval_main"),
          ("push_data_stack",),

          ("get_mem_stack",),
          ("cdr_mem",),
        )),

        ("bind_args", "return",
         (("cmp_lex","NIL"),
          ("op_branch_mem_lex","return"),

          ("bind_mem_stack",),

          ("cdr_mem",),
          ("op_exec","bind_args"),

          ("get_mem_stack",),

          ("car_mem",),
          ("bind",),
        )),

        ("eval_built-in", "return",
         (("exec",),
        )),

        ("eval_list", "return",
         (("bind_mem_stack",),

          # Built-in operator?
          #     T: execute built-in op sequence
          #     F: continue
          ("car_mem",),
          ("built-in?","eval_built-in"),

          ("get_mem_stack",),
          ("bind_mem_stack_ctx", "inst"),

          # Evaluate function object
          ("get_mem_stack_ctx", "inst"),
          ("car_mem",),
          ("op_exec","eval_main"),
          ("bind_mem_stack_ctx", "obj"),

          # If function, proceed to eval_func
          ("cmp_lex","#FUNCTION"),
          ("op_branch_mem_lex","eval_func"),

          # If object, interpret as get_attr
          ("cmp_lex","#OBJECT"),
          ("op_branch_mem_lex","get_attr_internal"),
        )),

        ("eval_func", "return",
          # Evaluate and push argument values
         (("get_mem_stack_ctx", "inst"),
          ("cdr_mem",),
          ("op_exec","push_args"),

          # New env if >0 arguments
          ("get_mem_stack_ctx", "obj"),
          ("get_mem_env",),
          ("car_mem",),
          ("op_exec","new_env_if_args"),
          ("get_mem_stack_ctx", "obj"),

          # New env from closure env
          #("get_mem_stack_ctx", "obj"),
          #("get_mem_env",),
          #("new_env",),

          # call if function
          ("op_exec","call_if_func"),

          # Retrieve old environment
          ("get_stack_env",),
        )),

        ("new_env_if_args", "return",
          # function?
          #     T: call
          #     F: error (eval_list -> error_exec)
         (# New env from closure env
          #("cmp_lex","NIL"),
          #("op_branch_mem_lex","return"),
          ("new_env",),
        )),

        ("call_if_func", "error_exec",
          # function?
          #     T: call
          #     F: error (eval_list -> error_exec)
         (("cmp_lex","#FUNCTION"),
          ("op_branch_mem_lex","call"),
        )),

        ("call", "progn",
         (("bind_mem_stack",),

          # Bind arguments
          ("car_mem",),
          ("op_exec","bind_args"),

          # Get function body
          ("get_mem_stack",),

          # Call via progn
        )),

        ("eval_main", "return",
         (
          ("cmp_lex","#LIST"),
          ("op_branch_mem_lex","eval_list"),

          # Built-in operators are self-evaluating
          ("built-in?","return"),

          # Hashes are self-evaluating
#          ("cmp_lex","#HASH"),
#          ("op_branch_mem_lex","return"),

          # Lookup variable and recover environment
          ("op_exec","find_binding"),
          ("lookup",),
          ("get_stack_env",),
        )),

        ("find_binding", "find_binding",
         (
          # op_branch (is it stable?)
          #     T: get mem
          #     F: recover env, shift to next
          ("check-env",),
          ("op_branch_bind","return"),
          ("prev_env",),

          # If advancement doesn't change, error
          ("op_branch_bind","error_lookup"),
        )),

        ("find_binding_or_def", "find_binding_or_def",
         (
          # op_branch (is it stable?)
          #     T: get mem
          #     F: recover env, shift to next
          ("check-env",),
          ("op_branch_bind","return"),
          ("prev_env",),

          # If advancement doesn't change, return default env
          ("op_branch_bind","return"),
        )),

        ########################################################################
        # Lists and other core operators
        ########################################################################

        ("cons", "return",
          # generate cons cell
          # push to data stack
          # recover instruction
         (("malloc_data_stack",),
          ("get_mem_stack",),

          ("cdr_mem",),
          ("op_exec","push_args"),
          ("pop_data_stack",),

          ("cons",),
          ("set_mem_lex","#LIST"),
        )),

        ("car", "return",
         (("cadr_mem",),
          ("op_exec","eval_main"),
          ("car_mem",),
        )),

        ("cdr", "return",
         (("cadr_mem",),
          ("op_exec","eval_main"),
          ("cdr_mem",),
        )),

        ("cadr", "return",
         (("cadr_mem",),
          ("op_exec","eval_main"),
          ("cadr_mem",),
        )),

        ("atom", "get_false_mem",
         (("cadr_mem",),
          ("op_exec","eval_main"),
          ("cmp_mem",),
          ("op_branch_mem_lex_loop", "get_true_mem"),
        )),

        ("listp", "get_false_mem",
         (("cadr_mem",),
          ("op_exec","eval_main"),
          ("cmp_lex","#LIST"),
          ("op_branch_mem_lex","get_true_mem"),
          ("cmp_lex","NIL"),
          ("op_branch_mem_lex","get_true_mem"),
        )),

        ("not", "return",
         (("cadr_mem",),
          ("op_exec","eval_main"),

          ("op_exec", "false_or_nil?"),
        )),

        ("and", "and",
         # loop through expressions and eval
         # if eval yields false, return false
         # if NIL is hit, return true
         (("cdr_mem",),
          ("cmp_lex", "NIL"),
          ("op_branch_mem_lex","get_true_mem"),

          ("bind_mem_stack",),
          ("car_mem",),
          ("op_exec","eval_main"),

          ("op_exec", "false_or_nil?"),
          ("cmp_lex","true"),
          ("op_branch_mem_lex","get_false_mem"),
          ("get_mem_stack",),
        )),

        ("or", "or",
         # loop through expressions and eval
         # if eval yields true, return true
         # if NIL is hit, return false
         (("cdr_mem",),
          ("cmp_lex", "NIL"),
          ("op_branch_mem_lex","get_false_mem"),

          ("bind_mem_stack",),
          ("car_mem",),
          ("op_exec","eval_main"),

          ("op_exec", "false_or_nil?"),
          ("cmp_lex","false"),
          ("op_branch_mem_lex","get_true_mem"),
          ("get_mem_stack",),
        )),

        ("eq", "get_false_mem",
         (("cdr_mem",),
          ("op_exec","push_args"),
          ("pop_data_stack",),
          ("eq?","get_true_mem"),
        )),

        ("quote", "return",
         (("cadr_mem",),
        )),

        # eval double evaluates first argument
        ("eval", "eval_main",
         (("cadr_mem",),
          ("op_exec", "eval_main"),
        )),

        ("list", "return",
         (("cdr_mem",),
          ("cmp_lex","NIL"),
          ("op_branch_mem_lex","return"),

          ("bind_mem_stack",),

          # create cons cell memory
          # push to stack
          ("malloc_data_stack",),

          # eval arg and push to stack
          ("get_mem_stack",),
          ("car_mem",),
          ("op_exec","eval_main"),
          ("push_data_stack",),

          # recurse on next arg 
          ("get_mem_stack",),
          ("op_exec","list"),

          ("cons",),
          ("set_mem_lex","#LIST"),
        )),

        # (longer? list_a list_b)
        # Returns true if list_a is longer than list_b
        # Returns false if either argument is not a list
        ("longer?", "get_false_mem",
         (("cdr_mem",),
          ("bind_mem_stack_ctx", "inst"),

          ("car_mem",),
          ("op_exec","eval_main"),
          ("bind_mem_stack_ctx", "left"),

          # First argument must be a non-empty list to be longer
          ("cmp_lex","#LIST"),
          ("op_branch_mem_lex","longer?_2"),
          # Otherwise, first argument is either not a list or is not longer
          # Return false
        )),

        ("longer?_2", "get_false_mem",
         (("get_mem_stack_ctx", "inst"),
          ("cadr_mem",),
          ("op_exec","eval_main"),
          ("bind_mem_stack_ctx", "right"),

          # If second argument is a non-empty list, run loop
          ("cmp_lex","#LIST"),
          ("op_branch_mem_lex","longer_loop"),
          # If second argument is NIL, first arg is longer
          ("cmp_lex","NIL"),
          ("op_branch_mem_lex","get_true_mem"),
          # If second argument is not a list, return false
        )),

        ("longer_loop", "longer_loop",
         (("get_mem_stack_ctx", "left"),
          ("cmp_lex","NIL"),
          ("op_branch_mem_lex","get_false_mem"),
          ("cdr_mem",),
          ("bind_mem_stack_ctx", "left"),

          ("get_mem_stack_ctx", "right"),
          ("cmp_lex","NIL"),
          ("op_branch_mem_lex","get_true_mem"),
          ("cdr_mem",),
          ("bind_mem_stack_ctx", "right"),
        )),

        ########################################################################
        # Loop constructs
        ########################################################################

        ("repl", "repl",
         (("op_exec","read"),
          ("op_exec","eval_main"),
          ("op_exec","print_switch"),
        )),

        ("loop", "loop_loop",
         (("cadr_mem",),
          ("bind_mem_stack",),
        )),

        ("loop_loop", "loop_loop",
         (("get_mem_stack",),
          ("op_exec","eval_main"),
        )),

        ("progn", "progn_loop",
         # Push NIL
         # Will be overwritten if there are args
         (("cdr_mem",),
          ("push_data_stack",),
        )),

        ("progn_loop", "progn_loop",
          # cdr
          # is NIL?
          #   T : return
          #   F : continue
         (("cmp_lex","NIL"),
          ("op_branch_mem_lex","progn_end"),

          # stash mem
          # car
          # eval
          ("bind_mem_stack",),
          ("car_mem",),
          ("op_exec","eval_main"),
          ("set_data_stack",),

          ("get_mem_stack",),
          ("cdr_mem",),
        )),

        ("progn_end", "return",
         (("pop_data_stack",),
        )),

        # (dolist (var list) body)
        # NOTE:
        #    unlike common lisp, the last binding of var
        #    is the last element in list, not NIL
        # (dolist (x '(a b c) x)) => c
        ("dolist", "dolist_loop",
          # create new environment
         (("new_env",),
          ("set_env",),

          ("cdr_mem",),
          ("bind_mem_stack",),

          # evaluate list and push to data stack
          ("car_mem",),
          ("cadr_mem",),
          ("op_exec","eval_main"),
          ("push_data_stack",),
        )),

        ("dolist_loop", "dolist_loop",
          # check next loop element
          # is NIL?
          #   T : dolist_end
          #   F : continue
         (("get_data_stack",),
          ("cmp_lex","NIL"),
          ("op_branch_mem_lex","dolist_return"),

          # stash value
          ("car_mem",),
          ("push_data_stack",),

          # bind var
          ("get_mem_stack",),
          ("car_mem",),
          ("car_mem",),
          ("bind",),

          # eval body
          ("get_mem_stack",),
          ("cdr_mem",),
          ("op_exec","dolist_body"),

          # advance list
          ("pop_data_stack",),
          ("cdr_mem",),
          ("push_data_stack",),
        )),

        ("dolist_body", "dolist_body",
          # check next body element
          # is NIL?
          #   T : return
          #   F : continue
         (
          ("cmp_lex","NIL"),
          ("op_branch_mem_lex","return"),

          ("bind_mem_stack",),
          ("car_mem",),
          ("op_exec","eval_main"),

          ("get_mem_stack",),
          ("cdr_mem",),
        )),

        ("dolist_return", "dolist_end",
         (("pop_data_stack",),

          # check for return form
          # evaluate if exists
          ("get_mem_stack",),
          ("car_mem",),
          ("cdr_mem",),
          ("cdr_mem",),
          ("cmp_lex","NIL"),
          ("op_branch_mem_lex","dolist_end"),
          ("car_mem",),
          ("op_exec","eval_main"),
        )),

        ("dolist_end", "return",
          # recover environment
          # prev_env gets env from stack first
         (("prev_env",),
        )),

        ########################################################################
        # Conditional evaluation
        ########################################################################

        ("if", "eval_main",
          # cdr
         (("cdr_mem",),

          # stash mem
          # car
          # eval
          ("bind_mem_stack",),
          ("car_mem",),
          ("op_exec","eval_main"),

          # false or NIL?
          #   T : check for else condition
          #   F : eval first arg (break loop on car)
          ("op_exec", "false_or_nil?"),
          ("cmp_lex","true"),
          ("op_branch_mem_lex","if_alt"),

          ("get_mem_stack",),
          ("cadr_mem",),
        )),

        ("if_alt", "eval_main",
         (("get_mem_stack",),
          ("cdr_mem",),
          ("cdr_mem",),

          # NIL else?
          #   T : return NIL
          #   F : eval
          ("cmp_lex","NIL"),
          ("op_branch_mem_lex","return"),

          ("car_mem",),
        )),

        ("cond", "progn",
          # cdr
          # is NIL?
          #   T : return
          #   F : continue
         (("get_mem_stack",),
          ("cdr_mem",),
          ("cmp_lex","NIL"),
          ("op_branch_mem_lex","return"),

          # stash mem
          # car car
          # eval
          ("bind_mem_stack",),
          ("car_mem",),
          ("car_mem",),
          ("op_exec","eval_main"),

          # false or NIL?
          # get stack memory
          #   T : continue (repeat on cdr)
          #   F : eval first arg (break loop on car)
          ("op_exec", "false_or_nil?"),
          ("cmp_lex","true"),
          ("op_branch_mem_lex","cond"),

          ("get_mem_stack",),
          ("car_mem",),
        )),

        ########################################################################
        # Binding operators
        ########################################################################

        ("let", "return",
          # create new environment
         (("new_env",),

          # advance to var/val list
          ("cdr_mem",),
          ("bind_mem_stack",),

          # bind
          ("car_mem",),
          ("op_exec","let_loop"),

          # evaluate body via progn
          ("get_mem_stack",),
          ("op_exec","progn"),

          # recovery environment
          ("get_stack_env",),
        )),

        ("let_loop", "let_loop",
          # cdr
          # is NIL?
          #   T : return
          #   F : continue
         (("cmp_lex","NIL"),
          ("op_branch_mem_lex","return"),

          # stash value
          ("bind_mem_stack",),
          ("car_mem",),
          ("cadr_mem",),
          ("op_exec","eval_main"),
          ("push_data_stack",),

          # bind var
          ("get_mem_stack",),
          ("car_mem",),
          ("car_mem",),
          ("bind",),

          # next
          ("get_mem_stack",),
          ("cdr_mem",),
        )),

        ("setq", "setq_loop",
         (# advance to var/val list
          ("cdr_mem",),
          ("push_data_stack",),
        )),

        ("setq_loop", "setq_loop",
          # cdr
          # is NIL?
          #   T : return last value
          #   F : continue
         (("cmp_lex","NIL"),
          ("op_branch_mem_lex","progn_end"),

          # stash value
          # overwrite current data stack value
          # push copy for bind operation
          ("bind_mem_stack",),
          ("cadr_mem",),
          ("op_exec","eval_main"),
          ("set_data_stack",),
          ("push_data_stack",),

          # bind var wherever most recently bound or default env
          ("get_mem_stack",),
          ("car_mem",),
          ("op_exec", "find_binding_or_def"),
          ("bind",),
          ("get_stack_env",),

          # next
          ("get_mem_stack",),
          ("cdr_mem",),
          ("cdr_mem",),
        )),

        # TODO:
        # functions without arguments don't create namespaces
        # a (var ...) call in such a function will create or modify
        #   a variable in the parent namespace, which could be problematic
        ("var", "var_loop",
         (# advance to var/val list
          ("cdr_mem",),
          ("push_data_stack",),
        )),

        ("var_loop", "var_loop",
          # cdr
          # is NIL?
          #   T : return last value
          #   F : continue
         (("cmp_lex","NIL"),
          ("op_branch_mem_lex","progn_end"),

          # stash value
          # overwrite current data stack value
          # push copy for bind operation
          ("bind_mem_stack",),
          ("cadr_mem",),
          ("op_exec","eval_main"),
          ("set_data_stack",),
          ("push_data_stack",),

          # bind var in current env
          ("get_mem_stack",),
          ("car_mem",),
          ("bind",),

          # next
          ("get_mem_stack",),
          ("cdr_mem",),
          ("cdr_mem",),
        )),

        ########################################################################
        # Functions
        ########################################################################

        ("bridge-lambda", "lambda",
         (("bind_mem_stack",),)),

        # (defun name (args ...) body)
        ("defun", "return",
         (
          ("bind_mem_stack_ctx", "label"),
          # Recursive eval of lambda (second arg)
          ("cdr_mem",),
          ("op_exec","lambda"),

          # Push to stack
          ("push_data_stack",),

          # Update environment
          # No new env because name is bound in current
          ("set_mem_env",),

          # Get label and bind to function
          # Bound in outer environment
          ("get_mem_stack_ctx", "label"),
          ("cadr_mem",),
          ("bind",),
        )),

        # Creates a new memory node
        # Points to "call" symbol
        # Represents the pair ( args . body )
        ("lambda", "return",
         (("bind_mem_stack_ctx", "inst"),
          ("malloc_data_stack",),

          ("get_mem_stack_ctx", "inst"),
          ("cadr_mem",),
          ("push_data_stack",),

          ("get_mem_stack_ctx", "inst"),
          ("cdr_mem",),
          ("cdr_mem",),

          ("cons",),
          ("set_mem_lex","#FUNCTION"),

          # Bind environment here
          ("set_mem_env",),
        )),

        ########################################################################
        # I/O
        ########################################################################

        ("print_list", "return",
         (("bind_mem_stack",),
          ("print_sym","("),
          ("op_exec","print_list_loop"),
          ("print_sym",")"),
          ("get_mem_stack",),
         )),

        ("print_list_loop", "return",
         (("bind_mem_stack",),

          ("car_mem",),
          ("op_exec", "print_switch"),

          ("get_mem_stack",),
          ("cdr_mem",),
          ("cmp_lex","NIL"),
          ("op_branch_mem_lex","return"),

          ("cmp_lex","#LIST"),
          ("op_branch_mem_lex","print_list_loop"),
          ("print_mem",),
        )),

        ("print_switch", "return",
         (("cmp_lex","#LIST"),
          ("op_branch_mem_lex","print_list"),
          ("print_mem",),
        )),

        ("print", "print_loop",
         (("cdr_mem",),
          ("push_data_stack",),
         )),

        ("print_loop", "print_loop",
         (
          ("cmp_lex", "NIL"),
          ("op_branch_mem_lex","progn_end"),

          ("bind_mem_stack_ctx", "inst"),
          ("car_mem",),
          ("op_exec","eval_main"),
          ("set_data_stack",),
          ("op_exec","print_switch"),

          ("get_mem_stack_ctx", "inst"),
          ("cdr_mem",),
         )),

        # Read symbol
        # Stash lex
        # Learn lex->cmp
        # lex->mem->lex
        # Eq?
        #     T: return
        #     F: gen memory
        ("read_sym", "return",
         (("read_lex_recog?","return"),
          ("gen_sym_mem",),
        )),

        ("read", "return",
         (("op_exec","read_sym"),
          ("cmp_lex","("),
          ("op_branch_mem_lex","read_list"),
          ("cmp_lex","'"),
          ("op_branch_mem_lex","read_quote"),
        )),

        ("read_list", "return",
         (
          # stash car
          ("op_exec","read"),
          ("bind_mem_stack",),

          # check for list termination
          ("cmp_lex",")"),
          ("op_branch_mem_lex","close_list"),

          # cons cell
          ("malloc_data_stack",),

          # retrieve stashed car
          ("get_mem_stack",),
          ("push_data_stack",),

          # cdr
          ("op_exec","read_list"),

          ("cons",),
          ("set_mem_lex","#LIST"),
        )),

        ("read_quote", "return",
         (
          # stash quoted expression
          ("op_exec","read"),
          ("bind_mem_stack",),

          # INNER CELL: exp - NIL

          # cons cell
          ("malloc_data_stack",),

          # retrieve stashed car
          ("get_mem_stack",),
          ("push_data_stack",),

          # cdr
          ("get_sym_mem", "NIL"),
          ("cons",),
          ("set_mem_lex","#LIST"),
          ("bind_mem_stack",),

          # OUTER CELL: quote - INNER CELL
          # use quote for car

          # cons cell
          ("malloc_data_stack",),

          ("get_sym_mem", "quote"),
          ("push_data_stack",),

          # cdr
          ("get_mem_stack",),
          ("cons",),
          ("set_mem_lex","#LIST"),
        )),

        ("close_list", "return",
         (("get_sym_mem", "NIL"),)),

        ########################################################################
        # Hash maps
        ########################################################################

        # (makehash parent (key1 val1) ...)
#        ("makehash", "makehash_loop",
#         (
#          ("bind_mem_stack_ctx", "inst"),
#          ("makehash",),
#          ("set_mem_lex","#HASH"),
#          ("bind_mem_stack_ctx", "hash"),
#
#          # bind to parent
#          ("push_data_stack",),
#
#          ("get_mem_stack_ctx", "inst"),
#          ("cdr_mem",),
#          ("bind_mem_stack_ctx", "inst"),
#
#          ("car_mem",),
#          ("op_exec", "eval_main"),
#          ("push_data_stack",),
#
#          ("get_sym_mem", "NIL"),
#          ("cons",),
#        )),
#
#        ("makehash_loop", "makehash_loop",
#         (
#          ("get_mem_stack_ctx", "inst"),
#          ("cdr_mem",),
#          ("cmp_lex","NIL"),
#          ("op_branch_mem_lex","makehash_return"),
#          ("bind_mem_stack_ctx", "inst"),
#
#          ("car_mem",),
#          ("op_exec","push_args"),
#          ("get_mem_stack_ctx", "hash"),
#          ("op_exec","sethash_helper"),
#        )),
#
#        ("makehash_return", "return",
#         (("get_mem_stack_ctx", "hash"),
#        )),
#
#        ("checkhash", "get_false_mem",
#         (("cdr_mem",),
#          ("op_exec","push_args"),
#          ("pop_data_stack",),
#          ("bind_mem_stack",),
#          ("checkhash",),
#          ("op_branch_mem","get_true_mem"),
#        )),
#
#        ("gethash", "return",
#         (("cdr_mem",),
#          ("op_exec","push_args"),
#          ("pop_data_stack",),
#          ("gethash",),
#        )),
#
#        ("sethash", "sethash_helper",
#         (("cdr_mem",),
#          ("op_exec","push_args"),
#          ("pop_data_stack",),
#        )),
#
#        ("sethash_helper", "return",
#         (("bind_mem_stack",),
#          ("sethash",),
#        )),
#
#        ("remhash", "return",
#         (("cdr_mem",),
#          ("op_exec","push_args"),
#          ("pop_data_stack",),
#          ("bind_mem_stack",),
#          ("remhash","NIL"),
#        )),

        ########################################################################
        # Classes
        ########################################################################

        ("verify_object", "error_object",
         (("cmp_lex","#OBJECT"),
          ("op_branch_mem_lex","return"),
        )),

        # (defclass Class Parent
        #    (defun Class (self args) body)   # constructor
        #    (defun method (self args) body)
        #    ...)
        ("defclass", "return",
         (("cdr_mem",),
          ("bind_mem_stack_ctx", "inst"),

          ### Class memory structure
          ("malloc_data_stack",),
          ("bind_mem_stack_ctx", "obj"),

          # binding for class name
          ("get_mem_stack_ctx", "inst"),
          ("car_mem",),
          ("bind",),
          ("push_data_stack",),

          # class -> name -> parent
          ("get_mem_stack_ctx", "inst"),
          ("car_mem",),
          ("push_data_stack",),
          ("get_mem_stack_ctx", "inst"),
          ("cadr_mem",),
          ("op_exec","eval_main"),
          ("cons",),

          # env bindings
          ("new_env",),
          ("set_env",),
          ("set_mem_env",),

          ### Parse methods
          # just execute defuns in class environment
          ("get_mem_stack_ctx", "inst"),
          ("cdr_mem",),
          ("op_exec","progn"),

          ("get_mem_stack_ctx", "obj"),
          ("set_mem_lex","#CLASS"),

          # recovery environment
          ("prev_env",),
        )),

        # (new Class args ...)
        ("new", "error_exec",
         (("cdr_mem",),
          ("bind_mem_stack_ctx", "inst"),

          # Create object memory state
          ("malloc_data_stack",),
          ("set_mem_lex","#OBJECT"),
          ("bind_mem_stack_ctx", "obj"),

          # Get class namespace
          ("get_mem_stack_ctx", "inst"),
          ("car_mem",),
          ("op_exec","find_binding"),
          ("lookup",),
          ("bind_mem_stack_ctx", "class"),
          ("get_mem_env",),

          # Bind new environment
          ("get_mem_stack_ctx", "obj"),
          ("new_env",),
          ("set_mem_env",),
          ("push_data_stack",),

          # bind object to class mem
          ("get_mem_stack_ctx", "class"),
          ("push_data_stack",),
          ("get_sym_mem", "NIL"),
          ("cons",),

          # Retrieve old environment
          ("get_stack_env",),

          # class?
          #     T: construct
          #     F: error (new -> error_exec)
          ("get_mem_stack_ctx", "class"),
          ("cmp_lex","#CLASS"),
          ("op_branch_mem_lex","construct"),
        )),

        # (super obj args ...)
        ("super", "error_exec",
         # eval object
         #   push to stack
         #   bind to "obj" on stack
         (("cdr_mem",),
          ("bind_mem_stack_ctx", "inst"),
          ("car_mem",),
          ("op_exec", "eval_main"),
          ("bind_mem_stack_ctx", "obj"),
          ("push_data_stack",),

          # get superclass
          ("car_mem",),
          ("cdr_mem",),
          ("bind_mem_stack_ctx", "class"),

          # class?
          #     T: construct
          #     F: error (new -> error_exec)
          ("get_mem_stack_ctx", "class"),
          ("cmp_lex","#CLASS"),
          ("op_branch_mem_lex","construct"),
        )),

        ("construct", "return",
         (
          # Evaluate and push argument values
          ("get_mem_stack_ctx", "inst"),
          ("cdr_mem",),
          ("op_exec","push_args"),

          # get constructor
          ("get_mem_stack_ctx", "class"),
          ("get_mem_env",),
          ("get_sym_mem", "init"),
          ("op_exec","find_binding"),
          ("lookup",),
          ("bind_mem_stack_ctx", "constructor"),

          # Call with object namespace
          ("get_mem_stack_ctx", "obj"),
          ("get_mem_env",),
          ("get_mem_stack_ctx", "constructor"),
          ("op_exec","call_if_func"),

          # Return object
          ("get_mem_stack_ctx", "obj"),

          # recover environment
          ("get_stack_env",),
        )),

        # (has_attr obj attr)
        ("has_attr", "return",
          # get attribute name
         (("cdr_mem",),
          ("bind_mem_stack_ctx", "inst"),
          ("cadr_mem",),
          ("op_exec","eval_main"),
          ("bind_mem_stack_ctx", "attr"),

          # eval object
          ("get_mem_stack_ctx", "inst"),
          ("car_mem",),
          ("op_exec","eval_main"),
          ("op_exec", "verify_object"),

          # get obj env
          ("get_mem_env",),

          # check for attribute
          ("get_mem_stack_ctx", "attr"),
          ("op_exec","has_attr_check"),

          # recover environment
          ("get_stack_env",),
        )),
        ("has_attr_check", "get_false_mem",
          # lookup
         (("check-env",),
          ("op_branch_bind","get_true_mem"),
        )),

        # (get_attr obj attr)
        ("get_attr", "get_attr_internal",
          # eval object
         (("cdr_mem",),
          ("bind_mem_stack_ctx", "inst"),
          ("car_mem",),
          ("op_exec","eval_main"),
          ("op_exec", "verify_object"),
          ("bind_mem_stack_ctx", "obj"),
        )),

        # assumes object has been processed and is on stack
        # memory points to cons cell before attribute
        ("get_attr_internal", "return",
         (
          # get attribute name
          ("get_mem_stack_ctx", "inst"),
          ("cadr_mem",),
          ("op_exec","eval_main"),
          ("bind_mem_stack_ctx", "attr"),

          # get obj env
          ("get_mem_stack_ctx", "obj"),
          ("get_mem_env",),

          # evaluate sub-expression in mem env
          ("get_mem_stack_ctx", "attr"),
          ("op_exec","eval_main"),

          # Recover old env
          ("get_stack_env",),
        )),

        ("set_attr", "set_attr_loop",
          # eval object
         (("cdr_mem",),
          ("bind_mem_stack_ctx", "inst"),
          ("car_mem",),
          ("op_exec","eval_main"),
          ("bind_mem_stack_ctx", "obj"),
          ("op_exec", "verify_object"),

          # advance to var/val list
          ("get_mem_stack_ctx", "inst"),
          ("cdr_mem",),
          ("push_data_stack",),
        )),

        ("set_attr_loop", "set_attr_loop",
          # cdr
          # is NIL?
          #   T : return last value
          #   F : continue
         (("cmp_lex","NIL"),
          ("op_branch_mem_lex","progn_end"),

          # stash value
          # overwrite current data stack value
          # push copy for bind operation
          ("bind_mem_stack_ctx", "inst"),
          ("cadr_mem",),
          ("op_exec","eval_main"),
          ("set_data_stack",),
          ("push_data_stack",),

          # bind var in object env
          ("get_mem_stack_ctx", "inst"),
          ("car_mem",),
          ("op_exec","eval_main"),
          ("bind_mem_stack_ctx", "attr"),

          ("get_mem_stack_ctx", "obj"),
          ("get_mem_env",),
          ("get_mem_stack_ctx", "attr"),
          ("bind",),

          # next
          ("get_mem_stack_ctx", "inst"),
          ("cdr_mem",),
          ("cdr_mem",),

          # recover environment
          ("get_stack_env",),
        )),

        ("get_method", "return",
         (("cdr_mem",),
          ("bind_mem_stack_ctx", "inst"),

          # eval object
          ("car_mem",),
          ("op_exec","eval_main"),
          ("push_data_stack",),
          ("op_exec", "verify_object"),

          # get class and begin search loop
          ("car_mem",),
          ("cmp_lex","#CLASS"),
          ("op_branch_mem_lex","get_method_loop"),
        )),

        ("get_method_loop", "error_exec",
         (("bind_mem_stack_ctx", "class"),
          ("get_mem_env",),

          # get method name
          ("get_mem_stack_ctx", "inst"),
          ("cadr_mem",),

          # lookup
          ("check-env",),
          ("op_branch_bind","return"),

          # advance to parent class and loop
          ("get_mem_stack_ctx", "class"),
          ("cdr_mem",),
          ("cmp_lex","#CLASS"),
          ("op_branch_mem_lex","get_method_loop"),
        )),

        ("send", "return",
         (("bind_mem_stack_ctx", "inst"),

          # get method
          # leaves object on stack
          # leaves method ready to lookup
          ("op_exec", "get_method"),
          ("lookup",),
          ("bind_mem_stack_ctx", "method"),

          # stash object
          # pop because object (self) is not an argument
          ("pop_data_stack",),
          ("bind_mem_stack_ctx", "obj"),

          # Retrieve old environment
          ("get_stack_env",),

          # Evaluate and push argument values
          ("get_mem_stack_ctx", "inst"),
          ("cdr_mem",),
          ("cdr_mem",),
          ("cdr_mem",),
          ("op_exec","push_args"),

          # New env if >0 arguments
          ("get_mem_stack_ctx", "obj"),
          ("get_mem_env",),
          ("get_mem_stack_ctx", "method"),
          ("car_mem",),
          ("op_exec","new_env_if_args"),

          # New env from object env
          #("get_mem_stack_ctx", "obj"),
          #("get_mem_env",),
          #("new_env",),

          # call if function
          ("get_mem_stack_ctx", "method"),
          ("op_exec","call_if_func"),

          # Retrieve old environment
          ("get_stack_env",),
        )),

        ("type", "get_nil_mem",
          # eval object
         (("cadr_mem",),
          ("op_exec","eval_main"),

          ("cmp_lex","#OBJECT"),
          ("op_branch_mem_lex","get_type"),
        )),

        ("get_type", "return",
         (("car_mem",),
        )),

        ("instance_of", "instance_of_loop",
          # get class mem state
         (
          ("bind_mem_stack_ctx", "inst"),
          ("cadr_mem",),
          ("op_exec", "eval_main"),
          ("bind_mem_stack_ctx", "target_class"),

          # get object type
          ("get_mem_stack_ctx", "inst"),
          ("cdr_mem",),
          ("op_exec","type"),
          ("bind_mem_stack_ctx", "curr_class"),

          # loop
        )),

        ("instance_of_loop", "instance_of_loop",
         (("push_data_stack",),
          ("get_mem_stack_ctx", "target_class"),
          ("eq?","get_true_mem"),

          ("get_mem_stack_ctx", "curr_class"),
          ("cdr_mem",),
          ("cmp_lex", "NIL"),
          ("op_branch_mem_lex","get_false_mem"),
          ("bind_mem_stack_ctx", "curr_class"),
        )),

        ("chain", "return",
         (("cdr_mem",),
          ("op_exec","push_args"),
          ("pop_data_stack",),
          ("chain",),
          ("get_stack_env",),
        )),

        ########################################################################
        # Exceptions
        ########################################################################

        ("raise", "return",
         (("pop_exc_stack",),
        )),

        ("try", "return",
         (("cdr_mem",),
          ("push_exc_stack", "handle"),
          ("car_mem",),
          ("op_exec", "eval_main"),
          ("drop_exc_stack",),
        )),

        ("handle", "eval_main",
         (("get_mem_stack",),
          ("get_stack_env",),
          ("cdr_mem",),
          ("cadr_mem",),
        )),
    ]

    gate_sequence = [
        # Begins a new gate sequence based on operation
        ("final_gh_op", None,
         ((("gh", "activate", "op", "hetero"),),)),

        # Executes a built-in instruction
        # Assumes that mem points to name in lex
        ("exec", "final_gh_op",
         (
             # 1. Get lex from mem (first element)
             # 2. Get expression memory prior to exec
             (("lex", "activate", "mem", "hetero"),
              ("mem", "activate", "stack", "hetero"),),

             (("op", "activate", "lex", "hetero"),),
         )),

        # Executes a child >operation<
        ("op_exec", "final_gh_op",
         (
             # This is the original that uses the fwd connection
             (("op", "stash"),
              ("bind", "stash"),
              ("lex", "activate", "op", "hetero"),),

             (("op", "learn", "stack", "hetero"),
              ("stack", "activate", "stack", "fwd"),),

             (("op", "activate", "lex", "hetero"),
              ("bind", "learn", "stack", "hetero"),),
         )),

        #
        ("prev_env", None,
         (
             (("bind", "activate", "stack", "hetero"),),
             (("gh", "learn", "bind", "hetero"),
              ("bind", "activate", "bind", "hetero"),
              ("op", "activate", "op", "hetero"),
              ("bind", "stash"),),
             (("bind", "learn", "stack", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        #
        ("new_env", None,
         (
             (("bind", "stash"),),
             (("bind", "noise"),
              ("op", "activate", "op", "hetero"),),
             (("bind", "learn", "bind", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        #
        ("set_env", None,
         (
             (("bind", "stash"),
              ("op", "activate", "op", "hetero"),),
             (("bind", "learn", "stack", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        ("op_branch_exc_stack", None,
         (
             (("gh", "activate", "exc_stack", "hetero"),
              ("gh", "bias", "false"),),
         )),

        ("op_branch_bind", None,
         (
             (("gh", "activate", "bind", "hetero"),
              ("gh", "bias", "false"),),
         )),

        ("op_branch_lex", None,
         (
             (("gh", "activate", "lex", "hetero"),
              ("gh", "bias", "false"),),
         )),

        ("op_branch_mem", None,
         (
             (("gh", "activate", "mem", "hetero"),
              ("gh", "bias", "false"),),
         )),

        ("op_branch_mem_lex", "op_branch_lex",
         (
             (("lex", "activate", "mem", "hetero"),),
         )),

        ("op_branch_mem_lex_loop", "op_branch_mem",
         (
             (("lex", "activate", "mem", "hetero"),),
             (("mem", "activate", "lex", "hetero"),),
         )),

        ("true", "final_gh_op",
         (
             (("lex", "activate", "op", "hetero"),),
             (("op", "activate", "lex", "hetero"),),
         )),

        ("false", "final_gh_op",
         ((("op", "activate", "op", "hetero"),),)),

        # 
        ("built-in?", "op_branch_lex",
         (
             (("lex", "activate", "mem", "hetero"),),
             (("gh", "learn", "lex", "hetero"),),
             (("lex", "activate", "lex", "auto"),),
         )),

        # 
        ("check-env", None,
         (
             # Get masked environment
             (("lex", "activate", "mem", "hetero"),),
             (("bind_ctx", "activate", "lex", "hetero"),),
             (("bind", "context", "bind_ctx"),
              ("op", "activate", "op", "hetero"),),

             # Memorize and converge
             (("gh", "learn", "bind", "hetero"),
              ("bind", "context", "bind_ctx"),
              ("bind", "activate", "bind", "auto"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # Assumes lex has stashed the right value
        # Creates new memory and bind_ctx patterns
        # Associates them with new lex pattern
        ("gen_sym_mem", None,
         (
             (("mem", "noise"),
              ("bind_ctx", "noise"),
              ("mem_ctx", "noise"),
              ("mem", "stash"),
              ("bind_ctx", "stash"),
              ("mem_ctx", "stash"),
              ("op", "activate", "op", "hetero"),),
             (("mem", "learn", "mem", "auto"),
              ("lex", "learn", "mem", "hetero"),),
             (("lex", "activate", "mem", "hetero"),),
             (("mem", "learn", "lex", "hetero"),
              ("bind_ctx", "learn", "lex", "hetero"),
              ("mem_ctx", "learn", "lex", "hetero"),
              ("mem_ctx", "learn", "mem", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # 
        ("cmp_lex", None,
         (
             (("lex", "activate", "op", "hetero"),
              ("op", "activate", "op", "hetero"),),
             (("gh", "learn", "lex", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        ("cmp_mem", "final_gh_op",
         (
             (("gh", "learn", "mem", "hetero"),
              ("op", "activate", "op", "hetero"),),
         )),

        ("get_sym_mem", None,
         (
             (("lex", "activate", "op", "hetero"),
              ("op", "activate", "op", "hetero"),),
             (("mem", "activate", "lex", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        #########
        ### STACK
        #########

        # Retrieves the operation bound to the stack
        ("get_stack_op", "final_gh_op",
         (
             (("op", "activate", "stack", "hetero"),),
             (("op", "activate", "op", "hetero"),),
         )),

        # Advances the stack backward
        ("adv_stack_prev", "final_gh_op",
         (
             (("stack", "activate", "stack", "bwd"),
              ("op", "activate", "op", "hetero"),),
         )),

        # Binds memory to stack
        ("bind_mem_stack", None,
         (
             (("mem", "stash"),
              ("op", "activate", "op", "hetero"),),
             (("mem", "learn", "stack", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # Binds memory to stack with context
        ("bind_mem_stack_ctx", None,
         (
             (("lex", "activate", "op", "hetero"),),
             (("mem_ctx", "activate", "lex", "hetero"),
              ("stack_ctx", "activate", "lex", "hetero"),),
             (("mem", "context", "mem_ctx"),
              ("stack", "context", "stack_ctx"),
              ("mem", "stash"),),
             (("mem", "learn", "stack", "hetero"),
              ("mem", "context", "mem_ctx"),
              ("op", "activate", "op", "hetero"),),
             (("mem", "converge"),
              ("stack", "converge"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # Gets memory from stack
        ("get_mem_stack", "final_gh_op",
         (
             (("mem", "activate", "stack", "hetero"),
              ("op", "activate", "op", "hetero"),),
         )),

        # Gets memory from stack with context
        ("get_mem_stack_ctx", None,
         (
             (("lex", "activate", "op", "hetero"),),
             (("mem_ctx", "activate", "lex", "hetero"),
              ("stack_ctx", "activate", "lex", "hetero"),),
             (("stack", "context", "stack_ctx"),),
             (("mem", "context", "mem_ctx"),
              ("mem", "activate", "stack", "hetero"),
              ("op", "activate", "op", "hetero"),),
             (("mem", "converge"),
              ("stack", "converge"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # Generate memory state and push onto stack
        ("malloc_data_stack", "push_data_stack",
         (
             (("mem", "noise"),
              ("mem", "stash"),),
             (("mem", "learn", "mem", "auto"),),
         )),

        # Overwrite top of data stack
        ("set_data_stack", None,
         (
             (("mem", "stash"),
              ("op", "activate", "op", "hetero"),),
             (("mem", "learn", "data_stack", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # Pushes current mem on data stack
        ("push_data_stack", None,
         (
             # This is the original that uses the fwd connection
             (("data_stack", "activate", "data_stack", "fwd"),
              ("mem", "stash"),
              ("op", "activate", "op", "hetero"),),
             (("mem", "learn", "data_stack", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # Gets top of data stack without popping
        ("get_data_stack", "final_gh_op",
         (
             (("mem", "activate", "data_stack", "hetero"),
              ("op", "activate", "op", "hetero"),),
         )),

        # Pops top of data stack
        ("pop_data_stack", "final_gh_op",
         (
             (("data_stack", "activate", "data_stack", "bwd"),
              ("mem", "activate", "data_stack", "hetero"),
              ("op", "activate", "op", "hetero"),),
         )),

        #######
        ### MEM
        #######

        # Transitions memory using current memory context
        ("transit_mem", None,
         (
             (("mem", "context", "mem_ctx"),),
             (("mem", "activate", "mem", "hetero"),
              ("mem", "context", "mem_ctx"),
              ("op", "activate", "op", "hetero"),),
             (("mem", "converge"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # Advances the memory layer using memory context
        ("car_mem", "transit_mem",
         (
             (("mem_ctx", "activate", "mem", "hetero"),),
         )),

        # Advances the memory layer twice using same memory context
        ("cdr_mem", "transit_mem",
         (
             (("mem_ctx", "activate", "mem", "hetero"),),
             (("mem", "context", "mem_ctx"),),
             (("mem", "activate", "mem", "hetero"),
              ("mem", "context", "mem_ctx"),),

             # TODO: do we need to converge here?
             (("mem", "converge"),),
         )),

        # Convenience for (car (cdr x))
        ("cadr_mem", "car_mem",
         (
             (("mem_ctx", "activate", "mem", "hetero"),),
             (("mem", "context", "mem_ctx"),),
             (("mem", "activate", "mem", "hetero"),
              ("mem", "context", "mem_ctx"),),

             # TODO: do we need to converge here?
             (("mem", "converge"),
              ("mem", "context", "mem_ctx"),),

             (("mem", "activate", "mem", "hetero"),
              ("mem", "context", "mem_ctx"),),
             (("mem", "converge"),),
         )),

        # 
        ("set_mem_lex", None,
         (
             (("lex", "activate", "op", "hetero"),),
             (("lex", "stash"),
              ("op", "activate", "op", "hetero"),),
             (("lex", "learn", "mem", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # 
        ("set_mem_env", None,
         (
             (("bind", "stash"),
              ("op", "activate", "op", "hetero"),),
             (("bind", "learn", "mem", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # 
        ("get_mem_env", "final_gh_op",
         (
             (("op", "activate", "op", "hetero"),
              ("bind", "activate", "mem", "hetero"),),
         )),

        # 
        ("get_stack_env", "final_gh_op",
         (
             (("op", "activate", "op", "hetero"),
              ("bind", "activate", "stack", "hetero"),),
         )),

        # 
        ("cons", None,
         (
             # Generate context
             # Stash contextualized cdr memory
             (("mem_ctx", "noise"),
              ("mem_ctx", "stash"),),
             (("mem", "context", "mem_ctx"),
              ("mem", "stash"),),

             # Retrieve and contextualize car memory
             # Bind car to cdr
             (("mem", "activate", "data_stack", "hetero"),
              ("mem", "context", "mem_ctx"),),
             (("mem", "context", "mem_ctx"),
              ("mem", "learn", "mem", "hetero"),
              ("mem", "stash"),
              ("data_stack", "activate", "data_stack", "bwd"),),

             # Retrieve and contextualize cons cell memory
             # Bind cons to car
             (("mem", "activate", "data_stack", "hetero"),
              ("mem", "context", "mem_ctx"),),
             (("mem", "learn", "mem", "hetero"),),

             # Bind cons to ctx and return cons cell
             (("mem", "activate", "data_stack", "hetero"),
              ("data_stack", "activate", "data_stack", "bwd"),
              ("op", "activate", "op", "hetero"),),
             (("mem_ctx", "learn", "mem", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # 
        ("eq?", "op_branch_mem",
         (
             # Pipeline
             # mem -l> cmp
             # ds -> mem -> cmp
             (("gh", "learn", "mem", "hetero"),
              ("mem", "activate", "data_stack", "hetero"),
              ("data_stack", "activate", "data_stack", "bwd"),),
         )),

        # Prints contents of memory
        ("print_mem", None,
         (
             (("lex", "activate", "mem", "hetero"),
              ("op", "activate", "op", "hetero"),),

             (("lex", "print"),
              ("gh", "activate", "op", "hetero"),),
        )),

        # Prints symbol directly from op
        ("print_sym", None,
         (
             (("lex", "activate", "op", "hetero"),
              ("op", "activate", "op", "hetero"),),

             (("lex", "print"),
              ("gh", "activate", "op", "hetero"),),
        )),

        # Reads input symbol
        ("read_lex_recog?", "op_branch_mem_lex",
         (
             (("lex", "read"),
              ("lex", "stash"),),
             (("gh", "learn", "lex", "hetero"),
              ("mem", "activate", "lex", "hetero"),),

             #(("mem", "converge"),),
        )),

        # 
        ("lookup", "final_gh_op",
         (
             (("lex", "activate", "mem", "hetero"),),
             (("mem_ctx", "activate", "lex", "hetero"),),
             (("mem", "activate", "bind", "hetero"),
              ("mem", "context", "mem_ctx"),
              ("bind", "activate", "stack", "hetero"),
              ("op", "activate", "op", "hetero"),),
             (("mem", "converge"),
              ("gh", "activate", "op", "hetero"),),
         )),

        # 
        ("bind", None,
         (
             # Get mask for variable name (active in mem)
             (("lex", "activate", "mem", "hetero"),),
             (("bind_ctx", "activate", "lex", "hetero"),
              ("mem_ctx", "activate", "lex", "hetero"),),

             # Get value from stack
             (("mem", "activate", "data_stack", "hetero"),),

             #(("mem", "converge"),),

             # Stash active mem (value)
             (("mem", "context", "mem_ctx"),
              ("mem", "stash"),),

             # Mask binding layer and stash
             (("bind", "context", "bind_ctx"),
              ("bind", "stash"),),

             # Learn binding (with stability)
             (("mem", "learn", "bind", "hetero"),
              ("bind", "learn", "bind", "auto"),
              ("bind", "activate", "stack", "hetero"),
              ("op", "activate", "op", "hetero"),),

             # Pop stack
             (("mem", "activate", "data_stack", "hetero"),
              ("data_stack", "activate", "data_stack", "bwd"),
              ("gh", "activate", "op", "hetero"),),
         )),

#        ("makehash", None,
#         (
#             # Generate mem and context
#             (("mem", "noise"),
#              ("mem", "stash"),
#              ("mem_ctx", "noise"),
#              ("mem_ctx", "stash"),
#              ("op", "activate", "op", "hetero"),),
#
#             # Learn connections
#             (("mem_ctx", "learn", "mem", "hetero"),
#              ("mem", "learn", "mem", "auto"),
#              ("gh", "activate", "op", "hetero"),),
#         )),
#
#        ("checkhash", "transit_mem",
#         (
#             # Memorize key
#             (("mem", "activate", "data_stack", "hetero"),
#              ("data_stack", "activate", "data_stack", "bwd"),),
#             (("gh", "learn", "mem", "hetero"),),
#
#             # Get key context
#             (("mem_ctx", "activate", "mem", "hetero"),),
#
#             # Get masked hash
#             (("mem", "activate", "stack", "hetero"),
#              ("mem", "context", "mem_ctx"),),
#
#
#             # Transition (transit_mem)
#         )),
#
#        ("gethash", "transit_mem",
#         (
#             # TODO: check for key?
#
#             # Get hash context
#             (("mem_ctx", "activate", "mem", "hetero"),),
#
#             # Get masked key
#             (("mem", "activate", "data_stack", "hetero"),
#              ("data_stack", "activate", "data_stack", "bwd"),
#              ("mem", "context", "mem_ctx"),),
#
#             # Transition (transit_mem)
#         )),
#
#        ("sethash", None,
#         (
#             # Get hash context
#             (("mem_ctx", "activate", "mem", "hetero"),),
#
#             # Stash masked value
#             (("mem", "activate", "data_stack", "hetero"),
#              ("mem", "context", "mem_ctx"),
#              ("mem", "stash"),),
#
#             # Get key
#             (("data_stack", "activate", "data_stack", "bwd"),),
#             (("mem", "activate", "data_stack", "hetero"),
#              ("mem", "context", "mem_ctx"),),
#
#             # Learn masked transit
#             (("mem", "learn", "mem", "hetero"),),
#
#             # Get key context
#             # Stash masked key
#             (("mem", "activate", "data_stack", "hetero"),),
#             (("mem_ctx", "activate", "mem", "hetero"),),
#             (("mem", "context", "mem_ctx"),
#              ("mem", "stash"),),
#
#             # Get hash
#             (("mem", "activate", "stack", "hetero"),
#              ("mem", "context", "mem_ctx"),),
#
#             # Learn masked transit
#             (("mem", "learn", "mem", "hetero"),
#              ("op", "activate", "op", "hetero"),),
#
#             # Return hash
#             (("mem", "activate", "stack", "hetero"),
#              ("data_stack", "activate", "data_stack", "bwd"),
#              ("gh", "activate", "op", "hetero"),),
#         )),
#
#        ("remhash", None,
#         (
#             # Get key context
#             (("mem", "activate", "data_stack", "hetero"),
#              ("data_stack", "activate", "data_stack", "bwd"),),
#             (("mem_ctx", "activate", "mem", "hetero"),),
#
#             # Get masked NIL
#             (("lex", "activate", "op", "hetero"),),
#             (("mem", "activate", "lex", "hetero"),
#              ("mem", "context", "mem_ctx"),
#              ("mem", "stash"),),
#
#             # Get masked hash
#             (("mem", "activate", "stack", "hetero"),
#              ("mem", "context", "mem_ctx"),),
#
#             # Learn transit
#             (("mem", "learn", "mem", "hetero"),
#              ("op", "activate", "op", "hetero"),),
#
#             # Return hash
#             (("mem", "activate", "stack", "hetero"),
#              ("gh", "activate", "op", "hetero"),),
#         )),

        # Pops top of exception stack
        ("init_exc_stack", "final_gh_op",
         (
             (("gh", "learn", "exc_stack", "hetero"),
              ("op", "activate", "op", "hetero"),),
         )),

        # 
        ("push_exc_stack", "final_gh_op",
         (
             # This is the original that uses the fwd connection
             (("exc_stack", "activate", "exc_stack", "fwd"),
              ("op", "stash"),
              ("stack", "stash"),
              ("data_stack", "stash"),),
             (("op", "learn", "exc_stack", "hetero"),
              ("stack", "learn", "exc_stack", "hetero"),
              ("data_stack", "learn", "exc_stack", "hetero"),
              ("op", "activate", "op", "hetero"),),
         )),

        # Pops top of exception stack
        ("pop_exc_stack", "final_gh_op",
         (
             (("exc_stack", "activate", "exc_stack", "bwd"),
              ("op", "activate", "exc_stack", "hetero"),
              ("stack", "activate", "exc_stack", "hetero"),
              ("data_stack", "activate", "exc_stack", "hetero"),),
             (("lex", "activate", "op", "hetero"),),
             (("op", "activate", "lex", "hetero"),),
         )),

        # Pops top of exception stack
        ("drop_exc_stack", "final_gh_op",
         (
             (("exc_stack", "activate", "exc_stack", "bwd"),
              ("op", "activate", "op", "hetero"),),
         )),

        # Chain object namespaces
        ("chain", None,
         (
             (("bind", "activate", "mem", "hetero"),
              ("mem", "activate", "data_stack", "hetero"),
              ("data_stack", "activate", "data_stack", "bwd"),
              ("bind", "stash"),),
             (("bind", "activate", "mem", "hetero"),
              ("op", "activate", "op", "hetero"),),
             (("bind", "learn", "bind", "hetero"),
              ("gh", "activate", "op", "hetero"),),
         )),
    ]

    mappings = {}

    def count_mappings(l):
        syms = set()
        for k,ms in mappings.items():
            try: syms = syms.union(set(m[k.index(l)] for m in ms))
            except ValueError: continue
        return len(syms)


    # Gate regions
    ms, sym_gates = gen_gh_mappings(gate_sequence, "gh")
    mappings.update(ms)
    gates = set(tuple(g) for gs in sym_gates.values() for g in gs)

    # Operations
    mappings.update(
        gen_op_mappings(
            operations, "op", "lex", "gh"))


    ### Lexical symbols

    # Extract symbol memory referred to by interpreter
    # TODO:
    #   search for operations that use op->lex->mem pathway
    interp_sym_mem = set()
    for name, post, seq in operations:
        for pair in seq:
            if pair[0] in ("get_sym_mem",
                           #"remhash",
                           "bind_mem_stack_ctx",
                           "get_mem_stack_ctx"):
                interp_sym_mem.add(pair[1])

    stack_ctx_syms = set()
    for name, post, seq in operations:
        for pair in seq:
            if pair[0] in ("bind_mem_stack_ctx", "get_mem_stack_ctx"):
                stack_ctx_syms.add(pair[1])

    # Built-in operation symbols
    builtins = set((
        "cons", "car", "cdr", "cadr", "list", "longer?",
        "quote", "eval",
        "print", "read",

        "lambda", "defun",
        "let", "setq", "var",

        "progn", "loop", "dolist",
        "pass",
        "error", "halt", "repl",

        "cond", "if",
        "eq", "atom", "listp",
        "not", "and", "or",
        "NIL", "true", "false",

        #"makehash", "checkhash", "gethash", "sethash", "remhash",

        "defclass", "new", "super",
        "get_attr", "set_attr", "has_attr",
        "send",
        "type", "instance_of",

        "raise", "try",

        "chain",
    ))
    mappings["lex", "lex", "auto"] = [ (x, x) for x in builtins ]
    mappings["op", "lex", "hetero"].extend([ (x, x) for x in builtins ])


    # Default environment bindings
    def_bindings = (
        #("x", "a"),
    )

    # TODO:
    # set up flash function to accept different pre/post ctx regions
    '''
    environments = {
        "def_env" : [bind for bind in def_bindings],
    }

    mappings["bind", "bind", "auto", "bind_ctx"] = [
        (env, env, var)
        for env,binds in environments.items()
        for val,var in binds
    ]
    mappings["mem", "bind", "hetero", "mem_ctx"] = [
        (val, env, var)
        for env,binds in environments.items()
        for val,var in binds
    ]
    '''

    # Base case for recursive environment lookup
    mappings["bind", "bind", "hetero"] = [
        ("def_env", "def_env"),
    ]

    # Memory (program)
    # Built-in symbols do not need mem representations because they will be
    #    created as necessary during evaluation
    mem_syms = set(interp_sym_mem).union(
        a for a,b in def_bindings).union(
        b for a,b in def_bindings)
    mappings.update(
        gen_mem_mappings(
            tokenize(prog),
            "mem", "mem_ctx", "lex",
            mem_syms))

    # Environment variable contexts
    # Learn all possible lexical symbols
    mappings["bind_ctx", "lex", "hetero"] = [
        (sym, sym) for mem,sym in mappings["mem", "lex", "hetero"]
    ]
    mappings["mem_ctx", "lex", "hetero"] = [
        (sym, sym) for mem,sym in mappings["mem", "lex", "hetero"]
    ]

    # Context bindings for stack retrieval
    mappings["stack_ctx", "lex", "hetero"] = [
        (sym, sym) for sym in stack_ctx_syms
    ]


    cap = {
        "mem": 64,
        "lex": 64,
        "bind": 128,
        "stack": 256,
        "data_stack": 64,
        "exc_stack": 32,
        "op": count_mappings("op"),
        "gh": count_mappings("gh"),
    }

    cap.update(capacity)


    #######
    # Building the network
    #

    controller_layers = ("op", "gh")
    ortho_layers = controller_layers + (
        ("data_stack", "exc_stack") if ortho else ())
        #("stack", "data_stack", "exc_stack") if ortho else ())

    # capacity = num_attractors * 16
    # does not include stack (mappings added below)
    for k in cap:
        if (k, k, "auto") in mappings:
            cap[k] += len(mappings[k, k, "auto"])

    sizes = {
        "mem": cap["mem"],
        "lex": cap["lex"],
        "stack": cap["stack"],
        "data_stack": cap["data_stack"],
        "exc_stack": cap["exc_stack"],
        "bind": cap["bind"],
        "op": cap["op"],
        "gh": cap["gh"],
    }

    # Runtime stacks
    # Before adjusting size for number of neurons
    mappings.update(gen_stack_mappings(
        "stack", int(sizes["stack"]),
        auto=True))
    mappings.update(gen_stack_mappings(
        "data_stack", int(sizes["data_stack"])))
    mappings.update(gen_stack_mappings(
        "exc_stack", int(sizes["exc_stack"])))

    for k in sizes:
        if k == "stack":
            sizes[k] *= 16
        elif k in ortho_layers:
            sizes[k] *= 2
        elif k == "mem":
            sizes[k] *= 8
        else:
            sizes[k] *= 16

    sizes.update(layer_sizes)
    sizes["mem_ctx"] = sizes["mem"]
    sizes["bind_ctx"] = sizes["bind"]
    sizes["stack_ctx"] = sizes["stack"]

    lams = {
        "mem_ctx": 0.25,
        "bind_ctx": 0.125,
        "stack_ctx": 0.25,
    }
    lams.update(ctx_lam)

    main_activator = sign_activator
    #main_activator = lambda x: tanh_activator(0.0001, x)

    layers = { }

    def add_layer(name):
        if name not in layers:
            sz = sizes[name]
            layers[name] = Layer(name,
                heaviside_activator(sz, lam=lams[name])
                    if "ctx" in name else main_activator(sz),
                sz,
                compare = (name == "gh"),
                ortho = (name in ortho_layers),
                emulate=emulate,
                check=check)
        return layers[name]

    # Parse the gates and infer the architecture
    for g in gates:
        # Converge implies auto-associative matrix
        if g[1] == "converge":
            g = (g[0], "activate", g[0], "auto")

        if len(g) == 4 and g[1] == "activate":
            to_layer, typ, from_layer, name = g

            args={}
            # Decay auto-associative matrices and disable diagonal weights
            if name == "auto":
                args["diag"] = False

                if to_layer != "lex":
                    args["decay"] = decay
            # Also decay hetero-associative matrix for mem and bind
            elif name == "hetero" and from_layer in ("mem", "bind") and to_layer != "gh":
                args["decay"] = decay

            # Comparison connections
            if to_layer == "gh" and from_layer not in ("gh", "op"):
                args["compare"] = True

            to_layer, from_layer = add_layer(to_layer), add_layer(from_layer)
            to_layer.add_connection(from_layer, name, args)

    # Build network
    net = Network()
    for layer in layers.values():
        net.add_layer(layer)
    net.greedy_device_allocation()

    # Print neuron and weight counts
    if verbose:
        print("Name  N  W")
        neurons = 0
        weights = 0
        dev_weights = {}
        dev_bytes = {}

        for layer in layers.values():
            weights_layer = 0
            bytes_layer = 0

            for conn in layer.connections.values():
                w_size = conn.get_num_weights()
                w_bytes = conn.get_num_bytes()
                weights += w_size
                weights_layer += w_size
                dev_weights[conn.device] = dev_weights.get(conn.device, 0) + w_size
                dev_bytes[conn.device] = dev_bytes.get(conn.device, 0) + w_bytes
                bytes_layer += w_bytes

            print("-" * 60)
            gb_layer = bytes_layer / (1024**3)
            print(" ".join("%10s" % x
                for x in (layer.name, layer.size, weights_layer,
                    ("%9.6f" % gb_layer)))
                + (("   lam = %f" % lams[layer.name]) if layer.name in lams else ""))

            print("-" * 60)
            for conn in layer.connections.values():
                w_bytes = conn.get_num_bytes()
                print("        ",
                    ("%25s" % str(conn.name[1:])),
                    ("%9.6f" % (w_bytes / (1024**3))),
                    conn.device)
            print()

            neurons += layer.size
        print("Total: ", " ".join("%10s" % x
            for x in (neurons, weights)))
        print("Device allocation:")
        for k in sorted(dev_weights):
            dw = dev_weights[k]
            memory = dev_bytes[k]
            gb = memory / (1024**3)
            print("%d: %15d %15d (%6.4f GB)" % (k, dw, memory, gb))
        print()

    #######

    # Learning the gate system
    if verbose:
        print("Flashing gates...")
        start_time = perf_counter()
    net.flash_gates(sym_gates, layers["gh"], emulate=emulate, verbose=verbose)

    if verbose:
        print("Flashing mappings...")
        for k,v in sorted(mappings.items(), key=lambda x:len(x[1])):
            k = k if len(k) == 4 else k + ("",)
            print(*("%10s" % s for s in k), len(v))
            #for x in v:
            #    print(x)

    for gate,ms in mappings.items():
        flash_mappings(gate, ms, net, count=3, verbose=verbose)

    if verbose:
        elapsed = perf_counter() - start_time
        print()
        print("... flashed in %fs" % elapsed)
        print()

    return net

def preprocess(code, strip_comments=True):
    if strip_comments:
        code = " ".join(
            l for l in code.split("\n")
            if not l.strip().startswith("#"))
    else:
        code = " ".join(
            l for l in code.split("\n"))

    # Separate out parentheses and quote marker
    for tok in "()'":
        code = code.replace(tok, ' %s ' % tok)
    return code.split()


def tokenize(code):
    toks = preprocess(code)
    if len(toks) == 0:
        return tuple()

    stack = [[]]
    for tok in toks:
        # Avoid double quote and quote close paren
        if "'" in stack[-1] and tok in "')":
            raise ValueError("Cannot quote %s symbol" % tok)

        if tok == "'":
            stack.append(["'"])
        else:
            if tok == '(':
                stack.append([])
            elif tok == ')':
                stack[-2].append(stack[-1])
                stack = stack[:-1]
            else:
                stack[-1].append(tok)

            if "'" in stack[-1]:
                stack[-2].append(["quote"] + stack[-1][1:])
                stack = stack[:-1]

    def to_tuple(exp):
        if type(exp) is list:
            return tuple(to_tuple(x) for x in exp)
        else:
            return exp

    return to_tuple(stack[0][0])

def test(prog = "", inputs=(), t=20000, verbose=False, debug=False, integrity=False, callback=None, **kwargs):
    if integrity:
        return test_memory_integrity(prog, inputs, t, verbose, debug, **kwargs)
    net = gen_machine(prog, verbose=verbose, **kwargs)

    if verbose:
        print("Running network...")

    # init and execute
    if prog == "":
        net.set_outputs({
            "op" : "init_repl",
            "gh" : "final_gh_op",
            "stack" : "0",
            "data_stack" : "0",
            "exc_stack" : "0",
            "bind" : "def_env",
        })
    else:
        net.set_outputs({
            "mem" : "main0",
            "op" : "init",
            "gh" : "final_gh_op",
            "stack" : "0",
            "data_stack" : "0",
            "exc_stack" : "0",
            "bind" : "def_env",
        })

    return net, net.run_auto(t, verbose=verbose, debug=debug, inputs=inputs, callback=callback)

def test_memory_integrity(prog = "", inputs=(), t=20000, verbose=False, debug=False, **kwargs):
    kwargs["emulate"] = True
    net = gen_machine(prog, verbose=verbose, **kwargs)

    if verbose:
        print("Emulating network to gather mappings...")

    # init and execute
    if prog == "":
        net.set_outputs({
            "op" : "init_repl",
            "gh" : "final_gh_op",
            "stack" : "0",
            "data_stack" : "0",
            "exc_stack" : "0",
            "bind" : "def_env",
        })
    else:
        net.set_outputs({
            "mem" : "main0",
            "op" : "init",
            "gh" : "final_gh_op",
            "stack" : "0",
            "data_stack" : "0",
            "exc_stack" : "0",
            "bind" : "def_env",
        })

    timesteps, output = net.run_auto(t, verbose=verbose, debug=debug, inputs=inputs)

    print()
    print("Mappings")
    print("%40s %6s %6s %6s" % ("Name", "Online", "Flash", "All"))

    flashed_mappings = {}
    online_mappings = {}
    all_mappings = {}
    for layer in net.layers.values():
        #if layer.name != "gh" and "stack" not in layer.name:
        #if layer.name != "gh":
        if True:
            for conn in layer.connections.values():
                #if True:
                if not conn.compare:
                    if not conn.all_mappings.empty():
                        flashed_mappings[conn.name] = conn.flashed_mappings
                        online_mappings[conn.name] = conn.online_mappings
                        all_mappings[conn.name] = conn.all_mappings
                        print("%40s %6d %6d %6d" % (conn.name,
                            len(conn.online_mappings.mappings),
                            len(conn.flashed_mappings.mappings),
                            len(conn.all_mappings.mappings)))

    # Adjust stack mappings
    # Only connection that is sometimes contextualized, sometimes not
    # Also only connection that has to_masking but not from_masking
    conn_name = ("mem", "stack", "hetero")
    if conn_name in all_mappings:
        replacement_mappings = Mappings(net.get_connection(*conn_name))
        map_dict = {}
        ctx_or_not = {}
        for x,y in all_mappings[conn_name].learned:
            from_sym = x.get_symbol()
            new_status = (y.ctx_state is not None)
            #print(from_sym, y.get_symbol(), new_status, y.ctx_state)
            flipped = (ctx_or_not.get(from_sym, new_status) != new_status)

            if (from_sym not in map_dict) or flipped:
                map_dict[from_sym] = []

            # replace lack of ctx with special ONES state
            if not new_status:
                y = y.mask(State(net.get_layer("mem_ctx"), sym="ONES"))

            # add mask to stack
            x = x.mask(State(
                net.get_layer("stack_ctx"),
                sym=y.ctx_state.get_symbol()))

            map_dict[from_sym].append((x,y))
            ctx_or_not[from_sym] = new_status
        for vs in map_dict.values():
            for x,y in vs:
                replacement_mappings.add(x, y)
                #print(x.tuple(), y.tuple())
            #print(x.tuple(), y.tuple())
        all_mappings[conn_name] = replacement_mappings
        online_mappings[conn_name] = replacement_mappings


    if ("bind", "bind", "auto") in all_mappings:
        bindings = {}
        namespaces = {}
        inv_namespaces = {}
        m = all_mappings["bind", "bind", "auto"].mappings
        for ((from_name,from_sym,from_ctx_name,from_ctx_sym),to_ctx_tuple),to_state in m.items():
            if from_sym not in namespaces:
                namespaces[from_sym] = []
            namespaces[from_sym].append(from_ctx_sym)
            bindings[from_ctx_sym] = bindings.get(from_ctx_sym, 0) + 1
        inv_sizes = {}
        for namespace, vs in namespaces.items():
            inv_sizes[len(vs)] = inv_sizes.get(len(vs), 0) + 1
            vs = tuple(sorted(vs))
            inv_namespaces[vs] = inv_namespaces.get(vs, 0) + 1

        print()
        print("Namespaces:")
        for k in sorted(inv_namespaces.keys(), key = lambda k: inv_namespaces[k]):
            print(k, inv_namespaces[k])
        print()

        print("Namespace sizes:")
        for k in sorted(inv_sizes.keys()):
            print(k, inv_sizes[k])
        print()

        print("Bindings:")
        for k in sorted(bindings.keys(), reverse=True, key=lambda k: bindings[k]):
            print(k, bindings[k])
        print()
        print()

    if verbose:
        print("Testing network mappings...")

    kwargs["emulate"] = False
    net = gen_machine(prog, verbose=verbose, **kwargs)

    # Add special ONES context state
    mem_ctx = net.get_layer("mem_ctx")
    mem_ctx.encode("ONES", np.ones((mem_ctx.size, 1), dtype=np.float32))
    stack_ctx = net.get_layer("stack_ctx")
    stack_ctx.encode("ONES", np.ones((stack_ctx.size, 1), dtype=np.float32))

    for name in sorted(all_mappings.keys(), key=lambda z: z[-1] != "auto"):
        #if name in (("mem", "mem", "auto"), ("mem", "stack", "hetero"), ("stack", "stack", "auto")):
        #if True:
        #if name[:2] == ("mem", "mem"):
        #if name[0] in ("mem", "bind"):

        if len(online_mappings[name].learned) < 20000:
            net.flash(name, online_mappings[name], count=1, verbose=True)
            net.test(name, all_mappings[name], converge = name[0] == "mem")

    return net, (0, [])
