import argparse
from build_neuroceril import *
from gnetwork import *
from gnetwork.network import State

import matplotlib.pyplot as plt
import numpy as np
from scipy.sparse import coo_matrix

def read_demo(name):
    path = "./demos/%s.lisp" % name
    try:
        return "".join(open(path).readlines())
    except:
        raise ValueError("Could not find demo %s" % path)

def read_kb(name):
    path = "./kbs/%s.lisp" % name
    try:
        return "".join(open(path).readlines())
    except:
        raise ValueError("Could not find knowledge base %s" % path)

# Y combinator
def test_y(args={}):
    prog = '''
        (progn
          (((lambda (le)
              ((lambda (g) (g g))
                   (lambda (h)
                          (le (lambda (x) ((h h) x))))))
                (lambda (f)
                    (lambda (x)
                        (if x
                            (progn
                                (print x)
                                (f (cdr x))
                                (print (car x)))))))
                #(quote (a b c d e f g h i j a b c d e f g h i j)))
                (quote (a b c)))
            (print (quote complete)))
    '''

    #return test(inputs=preprocess(prog),
    return test(prog,
        t=100000, verbose=args.verbose,
        debug=args.debug,
        capacity = {
            #"mem": 3500,
            "mem": 2500,
            #"mem": 100,
            "lex": 0,
            #"bind": 2500,
            "bind": 1800,
            #"bind": 128,
            "stack": 128,
            "data_stack": 32,
        },
        ctx_lam = { "mem_ctx" : 0.25, "bind_ctx" : 0.125, "stack_ctx" : 0.25 },
        ortho=args.ortho,
        emulate=args.emulate,
        integrity=args.integrity,
        check=args.check,
        decay=args.decay)


def test_custom(args={}):
    #prog = input("Enter program: ")
    inputs = preprocess(input("Enter program: "))

    return test(inputs=inputs,
        t=1000000, verbose=args.verbose,
        debug=args.debug,
        #layer_sizes = {
        #    "mem" : 32*32,
        #    #"lex" : 32*16,
        #    "bind" : 16*16
        #},

        capacity = {
            "mem": 256,
            "lex": 128,
            "bind": 64,
            "stack": 256,
            "data_stack": 256,
        },

        ctx_lam = { "mem_ctx" : 0.25, },
        ortho=args.ortho,
        emulate=args.emulate,
        integrity=args.integrity,
        check=args.check,
        decay=args.decay)

def test_file(args={}):
    filename = input("Enter filename: ")
    inputs = preprocess(" ".join(open(filename).readlines()))

    return test(inputs=inputs,
        t=1000000, verbose=args.verbose,
        debug=args.debug,

        capacity = {
            "mem": 256,
            "lex": 128,
            "bind": 64,
            "stack": 256,
            "data_stack": 256,
        },

        ctx_lam = { "mem_ctx" : 0.25, },
        ortho=args.ortho,
        emulate=args.emulate,
        integrity=args.integrity,
        check=args.check,
        decay=args.decay)

# TODO:
#   inheritance
#     classes are cons cells: (name_sym, parent_class)
#     parent can be null
#   fix instance_of
#     search through memory instead of environments
#   change attribute storage
#     add "self" arguments
#     update send and new
#     update constructor
#   add support for stack binding with ctx
#     update emulator
#       doesn't support separate bindings for separate target ctx
def test_class(args={}):
    inputs = preprocess("""
        (defclass Double NIL
            (defun init (self first second))
            (defun foo (first) first))
        (defclass Triple Double
            (defun init (self first second third)
                (super self first second)
                (set_attr self
                   'fourth 'foo)
                (print (send self to_list)))
            (defun to_list ()
                (list
                    first second third fourth))
        )

        (var x 'x y 'y z 'z)
        (var obj (new Triple x y z))

        (get_attr obj 'first)
        (let ((a 'a) (b 'b) (c 'c))
            (set_attr obj 'first a 'second b 'third c))
        (get_attr obj 'first)

        (type obj)

        (send obj to_list)
        (send obj foo 'bar)
        (get_attr obj 'first)

        (instance_of Triple obj)
        (instance_of Double obj)
        (instance_of Triple 'x)
        """)

    return test(inputs=inputs,
        t=1000000, verbose=args.verbose,
        debug=args.debug,
        #layer_sizes = {
        #    "mem" : 32*32,
        #    #"lex" : 32*16,
        #    "bind" : 16*16
        #},

        capacity = {
            "mem": 256,
            "lex": 64,
            "bind": 128,
            "stack": 64,
            "data_stack": 64,
        },

        ctx_lam = { "mem_ctx" : 0.25, },
        ortho=args.ortho,
        emulate=args.emulate,
        integrity=args.integrity,
        check=args.check,
        decay=args.decay)

def test_imitate(args={}):
    prog = """
        (progn

            (defclass ListMap NIL
                (defun init (self))

                (defun push (key val)
                    (set_attr self key
                        (cons
                            val
                            (try (eval key))))))

            (defclass Environment NIL
                (defun init (self __parent queries)
                    (if __parent
                        (chain self __parent))
                    (dolist (c queries)
                        (send self setr c)))

                (defun extend (key)
                    (if (has_attr self key)
                        (self key)
                        (set_attr self key
                            (new Environment (try (self key)) 'NIL))))

                (defun setr (query)
                    # key (car query)
                    # next (cdr query)
                    # rest (cdr next)
                    (let ((next (cdr query)))
                        (if (cdr (cdr query))
                            (send
                                (send self extend (car query))
                                setr (cdr query))
                            (set_attr self
                                (car query)
                                (cadr query))))))


            ####################################################################

            (defclass Timepoint NIL
                (defun init (self previous env)
                    (set_attr self
                       'best_action 'NIL
                       'hyps (new ListMap)))

                (defun add_action (action)
                    # Compare cdr of current best and proposed best
                    #    path traced back through actions/timepoints
                    (let ((new_path ((action 'start_time) 'best_action)))
                        (if (or (not best_action)
                                (not (longer? new_path (cdr best_action))))
                            (set_attr self 'best_action (cons action new_path)))))

                (defun trace ()
                    (let ((act (car best_action)))
                        (if act
                            (progn
                                (send (act 'start_time) trace)
                                (send act trace))))))

            (defclass Action NIL
                (defun init (self identity args start_time end_time source_act source_hyp))

                (defun trace ()
                    (send self print_action)
                    (if source_hyp
                        (progn
                            (print '+-->)
                            (send source_hyp trace)
                            (send source_act trace)
                            (print '+--<))))

                (defun print_action ()
                    (print identity args)))

            (defclass Recipe NIL
                (defun init (self cause effects)))

            (defclass Hypothesis NIL
                (defun init (self previous recipe start_time last_action)
                    (set_attr self
                       'subs 'NIL
                       'curr_eff
                           (if previous (cdr (previous 'curr_eff))
                               (recipe 'effects))))

                (defun trace ()
                    (if previous
                        (progn
                            (send previous trace)
                            (send last_action trace))))

                (defun last? ()
                    (eq 'NIL (cdr curr_eff)))
                (defun get_expected ()
                    (car (car (cadr curr_eff))))

                # Returns whether match was successful
                (defun match (action)

                    # Create substitution environment
                    (set_attr self
                        'subs (new Environment (try (previous 'subs)) 'NIL))
                    (set_attr subs 'env (start_time 'env))

                    (try
                        (and
                            # Simplified unification on action arguments
                            (let ((exp_list (cdr (car (car curr_eff))))
                                  (val 'NIL))
                                (dolist (obs (action 'args) 'true)

                                    (setq val (try (subs (car exp_list)) 'NOT_FOUND))
                                    (if (eq 'NOT_FOUND val)
                                       (set_attr subs (car exp_list) obs)
                                       (if (not (eq val obs)) (raise)))

                                    (setq exp_list (cdr exp_list))))

                            # Check logical predicate
                            (let ((predicate (cdr (car curr_eff))))
                                (if predicate
                                    (subs (car predicate))
                                    'true)))))

                (defun gen_action (source_act)
                    (new Action
                        (car (recipe 'cause))
                        (subs
                            (cons 'list
                                (cdr (recipe 'cause))))
                        start_time
                        (source_act 'end_time)
                        source_act
                        self))

                (defun extend (action)
                    (new Hypothesis self recipe start_time action)))

            (defun process_hypothesis (hyp act_obj)
                # if match (unification) and precondition
                #     if complete (last expected action)
                #         generate and return action
                #     else
                #         extend hypothesis
                # else retain hypothesis (do nothing)
                (if (send hyp match act_obj)
                    (if (send hyp last?)
                        (process_action (send hyp gen_action act_obj))
                        (send ((act_obj 'end_time) 'hyps)
                            push (send hyp get_expected)
                            (send hyp extend act_obj))
                    )
                )
            )

            # Create causal knowledge base
            (var knowledge_base (new ListMap))

            (defun process_action (act_obj)
                (var identity (act_obj 'identity))
                (send (act_obj 'end_time) add_action act_obj)

                (print '=====-PROCESS_ACTION-=====)
                (print identity)

                (var new_act)
                (var start_time (act_obj 'start_time))

                # EVOKE
                (dolist (recipe (try (knowledge_base identity)))
                    (process_hypothesis
                        (new Hypothesis 'NIL recipe start_time 'NIL)
                        act_obj))

                # EXTEND
                (dolist (hyp (try ((start_time 'hyps) identity)))
                    (process_hypothesis
                        hyp
                        act_obj))
            )

            #######################

            # Load causal recipes into knowledge base
            %s

            (dolist (recipe recipes)
                (send knowledge_base push
                    (car (car (car (recipe 'effects))))
                    recipe))

            (print 'BEGIN_DEMO)

            # DEMO
            # Explain demonstration
            # Current timepoint (contains hypothesis set)
            (let ((curr_env (eval (read)))
                  (curr_t (new Timepoint 'NIL curr_env)))
                (defun process_demo (data)
                    (if data
                        (let ((act (car data))
                              (changes (cdr data)))
                            (if changes
                                (setq curr_env (new Environment curr_env changes)))
                            (print '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
                            (process_action
                                (new Action
                                    (car act)
                                    (cdr act)
                                    curr_t
                                    (setq curr_t
                                        (new Timepoint curr_t curr_env))
                                    'NIL
                                    'NIL))
                        )
                    (raise))
                )

                # Loop over demonstration until exception
                (try
                    (loop
                        (process_demo (read))))

                # Print explanatory trace
                (print '___)
                (print 'EXPLANATION)
                (print '___)

                #(send curr_t trace)
                (dolist (act (curr_t 'best_action))
                    (send act print_action))
            )

            (print 'done)
        )
        """ \
        % read_kb(args.kb_name)


    hist = []
    intervals = []

    first_time = {}
    last_time = {}

    begin = [False]

    demo_mem = set()

    def cb(net, curr_t, gates):
        if ("lex", "print") in gates:
            if net.get_layer("lex").outputs.get_symbol() == "BEGIN_DEMO":
                begin[0] = True

        if begin[0]:
            if ("mem", "learn", "mem", "auto") in gates or ("mem", "converge") in gates:
                sym = net.get_layer("mem").outputs.get_symbol()

                if ("mem", "learn", "mem", "auto") in gates:
                    demo_mem.add(sym)

                if sym in demo_mem:
                    if sym not in first_time:
                        first_time[sym] = len(hist)
                    else:
                        # count unique memories activated since last time
                        intervals.append((sym, len(set(s for t,s in hist[last_time[sym]+1:]))))

                        # count activations since last time
                        #intervals.append((sym, len(hist) - last_time[sym]))
                    last_time[sym] = len(hist)

                    hist.append((curr_t, sym))


    inputs=preprocess(read_demo(args.demo_name) + " NIL")
    net, out = test(prog,
        inputs=inputs,
        callback=cb,
        #t=1, verbose=args.verbose,
        #t=10000, verbose=args.verbose,
        t=10000000, verbose=args.verbose,
        debug=args.debug,

        layer_sizes = {
            "mem": 40000,
            "bind": 35000,

            #"mem": 1000,
            #"bind": 1000,
        },
        capacity = {
            # DOCK EMPIRICAL
            # UM THEORETICAL (sorta)
            # UM EMPIRICAL

            # (8 vs 16 multiplier)
            #"mem": 300,
            "mem": 3700,

            #"bind": 500,
            "bind": 2200,

            "lex": 256,
            "stack": 128,
            "data_stack": 128,
            "exc_stack": 32,
        },

        ctx_lam = { "mem_ctx" : 0.25, "bind_ctx" : 0.125, "stack_ctx" : 0.25 },
        #ctx_lam = { "mem_ctx" : 0.125, "bind_ctx" : 0.25, "stack_ctx" : 0.25 },
        ortho=args.ortho,
        emulate=args.emulate,
        integrity=args.integrity,
        check=args.check,
        decay=args.decay)


    # Get mem objects
    #   -> link to class name (lex)
    #   -> link to namespace
    inc_classes = None
    objs = {}
    syms = []
    non_obj = {}

    mem_layer = net.get_layer("mem")
    bind_mem = net.get_connection("bind", "mem", "hetero")
    lex_mem = net.get_connection("lex", "mem", "hetero")

    mem_het = net.get_connection("mem", "mem", "hetero")
    mem_ctx = net.get_connection("mem_ctx", "mem", "hetero")

    def car(mem_state):
        c = mem_ctx.lookup(mem_state)
        return State(mem_layer, mem_het.lookup(mem_state.mask(c), c).get_symbol())

    mem_activ_hist = mem_layer.activ_history
    for (x,c),y in lex_mem.all_mappings.mappings.items():
        y_sym = y.get_symbol()

        if y_sym == "#OBJECT":
            obj = State(mem_layer, x[1])

            namespace = bind_mem.lookup(obj).get_symbol()

            class_obj = car(obj)
            class_name = car(class_obj)
            class_sym = lex_mem.lookup(class_name).get_symbol()
            count = mem_activ_hist.get(x[1], 0)

            if inc_classes is None or class_sym in inc_classes:
                objs[x[1]] = (class_sym, count, namespace)
        elif y_sym[0] == "#":
            non_obj[x[1]] = y_sym
            #if y_sym in non_obj:
            #    non_obj[y_sym].append(x[1])
            #else:
            #    non_obj[y_sym] = [x[1]]
        else:
            syms.append(x[1])


    class_colors = {
        "Action" : "red",
        "Environment" : "red",
        "Hypothesis" : "red",
        "ListMap" : "red",
        "Timepoint" : "red",

        "Recipe" : "green",
        "#CLASS" : "green",
        "#FUNCTION" : "green",

        "#LIST" : "blue",
    }

    def get_color(sym):
        if sym in objs:
            return class_colors[objs[sym][0]]
        elif sym in non_obj:
            return class_colors[non_obj[sym]]
        elif sym in syms:
            return "green"

    #plt.bar(range(len(intervals)), [i[1] for i in intervals],
    #    color = [get_color(i[0]) for i in intervals])
    #plt.hist(intervals, log=True, bins=100)
    plt.show()

    data, rows, cols = [], [], []
    visited = {}
    for i,(t,sym) in enumerate(hist):
        if sym not in visited:
            visited[sym] = len(visited)
        data.append(1)
        rows.append(visited[sym])
        cols.append(i)

    demo_mem = sorted(set(first_time.keys()), key= lambda x:first_time[x])

    xmin = [first_time[sym] for sym in demo_mem]
    xmax = [last_time[sym] for sym in demo_mem]
    #print("First/Last:")
    #for a,b,c in zip(demo_mem, xmin, xmax):
    #    print(a, b, c)
    #print()
    plt.hlines(visited.keys(), xmin, xmax,
        colors = [get_color(k) for k in visited.keys()])
    plt.show()

    #lifespan = [last_time[sym] - first_time[sym] for sym in demo_mem]
    #plt.hist(lifespan, bins=100)
    #plt.show()

    all_counts = []
    counts = []
    for i in range(len(hist)):
        count = 0
        all_count = 0
        for sym in last_time:
            if i >= first_time[sym]:
                all_count += 1
                if i <= last_time[sym]:
                    count += 1
        counts.append(count)
        all_counts.append(all_count)
    print("Tracked memories:", len(last_time))
    print("Max load:", max(counts))
    print()
    plt.plot(counts)
    plt.plot(all_counts)
    plt.show()

    data = [(a,b) for a,b in zip(counts, all_counts)]
    output = []
    for i,(a,b) in enumerate(data):
        if i == 0 or i == (len(data)-1):
            output.append((i,a,b))
        elif not (data[i-1] == data[i] == (a,b)):
            output.append((i,a,b))
    #for i,a,b in output:
    #    print(i,a,b)
    #print(len(data), len(output))

    return net, out

def draw(net):
        draw_object_network(net,
            #("Hypothesis", "Timepoint", "Action"),
            #("Timepoint", "Action"),
            #("Timepoint",),
            #("Environment", "Timepoint", "Hypothesis"),
            labels=False,
            skip_graph=True)

        '''
        draw_virtual_network(net,
            layer_grid = [
                #["data_stack", "bind"],
                #["mem", "lex"],
                #["stack", "op"],
                #[None, "gh"],

                #["bind", "mem", "lex", "op", "gh"],

                #["bind", "stack", "data_stack"],
                #["mem", "lex", "op"],

                #["bind", "op"],
                #["mem", "lex"],

                #["bind", "mem", "lex", "op", "gh"],

                #["bind", "mem", "lex"],
                ["bind", "mem"],
                #["bind"],
                #["op", "gh"],
                #["mem"],
                #["mem", "lex"],
                #["op"],
                #["mem"],
                #["mem", "bind"],

                #["data_stack", "mem", "stack"],
                #[None, "bind", None],
            ],
            mix_layers = False,
            labels = False,
            include_flashed = False,
            recurrent = True)
        '''
        #draw_physical_network(net)

def check_wasted(net):
    print()
    print("Analyzing memory space...")
    print()

    mem = net.get_layer("mem")
    mem_auto_conn = net.get_connection("mem", "mem", "auto")
    mem_hetero_conn = net.get_connection("mem", "mem", "hetero")
    lex_conn = net.get_connection("lex", "mem", "hetero")
    mem_ctx_conn = net.get_connection("mem_ctx", "mem", "hetero")

    mem_states = [k[0][1] for k in mem_auto_conn.mappings.keys()]
    labels = {
        #m : lex_conn.mappings[State(mem, sym=m).tuple(), None].get_symbol()
        m : lex_conn.lookup(State(mem, sym=m)).get_symbol()
        for m in mem_states }

    conses = {}

    for k,v in labels.items():
        if v == "#LIST":
            ctx_state = mem_ctx_conn.lookup(State(mem, sym=k))
            state = State(mem, sym=k, ctx_state=ctx_state)
            car = mem_hetero_conn.lookup(state, to_ctx=ctx_state)
            cdr = mem_hetero_conn.lookup(car, to_ctx=ctx_state)
            if car.get_symbol() is None or cdr.get_symbol() is None:
                raise RuntimeError
            conses[k] = (car.get_symbol(), cdr.get_symbol())


    def trace(k):
        if k in conses:
            car, cdr = conses[k]
            return (trace(car),) + trace(cdr)
        elif labels[k] == "NIL":
            return ()
        else:
            return labels[k]

    traces = { k : trace(k) for k,v in labels.items() if v == "#LIST" }
    inv_traces = { }

    for k,v in traces.items():
        inv_traces[v] = inv_traces.get(v, []) + [k]

    wasted = 0
    for v,k in inv_traces.items():
        if len(k) > 1:
            wasted += len(k) - 1
            print("%3d : %s" % (len(k), v))

    lists = tuple(v for v in labels.values() if v == "#LIST")
    funcs = tuple(v for v in labels.values() if v == "#FUNCTION")
    objects = tuple(v for v in labels.values() if v == "#OBJECT")
    syms = tuple(v for v in labels.values() if v[0] != "#")

    print("Memory states:", len(labels))
    print("Symbols:", len(syms))
    print("Functions:", len(funcs))
    print("Objects:", len(objects))
    print("Lists:", len(lists))
    print("Wasted:", wasted)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-t', type=str, default="y",
                        help='test to run')
    parser.add_argument('-k', type=str, default="kb_il_streamlined",
                        dest='kb_name',
                        help='causal knowledge base')
    parser.add_argument('-n', type=str, default="demo_replace_red_with_spare_1",
                        dest='demo_name',
                        help='ceril demo to run')
    parser.add_argument('-v', action='store_true', default=False,
                        dest='verbose',
                        help='verbose output during execution')
    parser.add_argument('-x', action='store_true', default=False,
                        dest='debug',
                        help='debugging output of network activity')
    parser.add_argument('-m', action='store_true', default=False,
                        dest='emulate',
                        help='emulate network activity')
    parser.add_argument('-i', action='store_true', default=False,
                        dest='integrity',
                        help='test memory integrity')
    parser.add_argument('-c', action='store_true', default=False,
                        dest='check',
                        help='check network activity')
    parser.add_argument('-o', action='store_true', default=False,
                        dest='ortho',
                        help='orthogonal matrices for interpreter')
    parser.add_argument('-g', action='store_true', default=False,
                        dest='draw',
                        help='draw graph of attractor space')
    parser.add_argument('-d', type=float, default=1.0,
                        dest="decay",
                        help="weight decay")
    parser.add_argument('-w', action='store_true', default=False,
                        dest='wasted',
                        help='analyze memory space and check for redundant states')
    args = parser.parse_args()

    print("test: ", args.t)
    print("args: ")
    for arg in vars(args):
         print("  %10s : %s" % (arg, getattr(args, arg)))
    print()

    if args.decay < 0.0 or args.decay > 1.0:
        raise ValueError("Decay must be between 0 and 1")

    try:
        net, (timesteps, output) = {
            "y": test_y,
            "custom": test_custom,
            "file": test_file,
            "class" : test_class,
            "imitate" : test_imitate,
        }[args.t](args=args)
        print("Ran test '%s' in %d timesteps" % (args.t, timesteps))
        print("Output:")
        print(" ".join(o[1] for o in output))
    except KeyError:
        raise ValueError("Unrecognized test: %s" % args.t)

    if args.draw:
        draw(net)
    if args.wasted:
        check_wasted(net)
