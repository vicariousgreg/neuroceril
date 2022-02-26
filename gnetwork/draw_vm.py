import networkx as nx
import matplotlib.pyplot as plt
import numpy as np
from gnetwork.network import State

def draw_physical_network(net, labels=True):
    edge_density = {}
    node_density = {
        l.name : len(l.coder) for l in net.layers.values()
    }

    for l in net.layers.values():
        for conn in l.connections.values():
            k = (conn.to_layer.name, conn.from_layer.name)
            edge_density[k] = edge_density.get(k, 0) + len(conn.learned)

    for g in net.gates:
        if g[1] == "context":
            print(g)
            edge_density[g[0],g[2]] = 1

    edges = [(fl,tl) for (tl,fl) in edge_density.keys()]
    print("\nNodes:")
    for n in node_density:
        print(n)
    print("\nEdges:")
    #for e in sorted(edges, key= lambda x:(x[1],x[0])):
    for e in sorted(edges, key= lambda e:edge_density.get(e,0)):
        print("%5d: %10s -> %-10s" % ((edge_density.get(e,0),) + e))
    print()

    g = nx.DiGraph()
    g.add_edges_from(edges)
    #pos=nx.kamada_kawai_layout(g)
    #pos=nx.fruchterman_reingold_layout(g, pos=pos)
    ##pos=nx.planar_layout(g)
    ##pos=nx.circular_layout(g)

    pos=nx.fruchterman_reingold_layout(g)

    pos=nx.kamada_kawai_layout(g, pos=pos)

    node_colors = [
        'red' if "_ctx" in l else 'green'
        for l in g.nodes()]

    for (fl,tl) in edges:
        if "_ctx" in fl:
            edge_density[(tl,fl)] = node_density[fl]

    edge_widths = [edge_density.get((tl,fl), 0) for (fl,tl) in g.edges()]
    edge_widths = [5 * w / max(edge_widths) for w in edge_widths]
    edge_widths = [w+0.5 for w in edge_widths]

    node_sizes = [node_density.get(tl, 0) for tl in g.nodes()]
    node_sizes = [300 * w / max(node_sizes) for w in node_sizes]

    if labels:
        nx.draw_networkx_labels(g, pos, font_size=8)
    nx.draw_networkx_nodes(g, pos, node_color=node_colors, node_size=node_sizes)
    nx.draw_networkx_edges(g, pos, alpha=1.0, width=edge_widths)

    plt.show()

def draw_virtual_network(net, layer_grid=None, mix_layers=False,
        include_flashed=True, labels=True, recurrent=True):
    mappings = {
        conn.name : [(v.tuple()[1],k[0][1])
                for k,v in conn.all_mappings.mappings.items()
                    if include_flashed or (k not in conn.flashed_mappings.mappings)]
            for l in net.layers.values()
                for conn in l.connections.values()
                     if recurrent or (conn.from_layer != l)
#                    if conn.to_layer.name != "gh" or conn.from_layer.name in ("op", "gh")
    }

    if layer_grid is None:
        layer_grid = [["mem", "lex", "op", "gh"]]
    only_layers = [l for row in layer_grid for l in row if l is not None]

    edges = []
    for (tl,fl,name),m in mappings.items():
        if only_layers is not None and any(
            l not in only_layers for l in (tl,fl)): continue

        for syms in m:
            tsym, fsym = syms[:2]
            fnode = ("%s_%s" % (fl,fsym))
            tnode = ("%s_%s" % (tl,tsym))
            if fsym != tsym or tl != fl:
                edges.append((fl,tl,fnode,tnode))

    layers = only_layers if only_layers is not None else set(
        a for a,b,c,d in edges).union(set(b for a,b,c,d in edges))
    colors = ["red", "blue", "green", "orange",
              "purple", "brown", "black", "yellow"]
    color_map = {}

    poss = {}
    gs = {}
    for j,ls in enumerate(reversed(layer_grid)):
        for i,to_layer in enumerate(ls):
            g = nx.DiGraph()
            nodes = set(c for a,b,c,d in edges if a == to_layer).union(
                set(d for a,b,c,d in edges if b == to_layer))
            g.add_nodes_from(nodes)
            g.add_edges_from([(c,d)
                for a,b,c,d in edges
                    if a == to_layer and b == to_layer])

            color_map[to_layer] = colors[len(color_map)%len(colors)]

            print("%10s %5d %s" % (to_layer, len(nodes), color_map[to_layer]))
            #for node in sorted(nodes):
            #    print(node)

            if len(nodes) > 0:
                d = tuple(d for n,d in g.degree())
                std_d = np.std(d)
                if std_d != 0.:
                    it = int(20. * (np.mean(d) / np.std(d)))
                else:
                    it = 20

                try:
                    pos=nx.planar_layout(g)
                    pos=nx.fruchterman_reingold_layout(g, pos=pos)
                except:
                    #pos=nx.kamada_kawai_layout(g,weight=0.5)
                    try:
                        pos=nx.fruchterman_reingold_layout(g)
                    except:
                        pos=nx.kamada_kawai_layout(g,weight=0.5)
                #pos=nx.circular_layout(g)
                gs[to_layer] = g

                if len(nodes) == 1:
                    for k,v in pos.items():
                        poss[k] = v + np.array([
                            coeff_x*i, coeff_y*j])
                elif len(nodes) > 1:
                    xs = [v[0] for v in pos.values()]
                    ys = [v[1] for v in pos.values()]
                    x_min,y_min = (min(xs),min(ys))
                    x_range, y_range = max(xs)-x_min, max(ys)-y_min

                    def renorm(v):
                        return np.array((
                            (v[0] - x_min) / x_range,
                            (v[1] - y_min) / y_range))

                    coeff_x=1.5
                    coeff_y=1.5

                    for k,v in pos.items():
                        poss[k] = renorm(v) + np.array([
                            coeff_x*i, coeff_y*j])

    g = nx.DiGraph()

    # Update node positions based on inter-layer edges
    if mix_layers:
        g.add_edges_from(((c,d) for a,b,c,d in edges))
        gd = {n:d for n,d in g.degree()}

        for h in gs.values():
            poss.update(nx.kamada_kawai_layout(g,pos=poss))
            #poss.update(nx.fruchterman_reingold_layout(g, pos=poss))

    # Draw layer graphs
    for to_layer,h in gs.items():
        node_colors = color_map[to_layer]

        #degrees = [len(tuple(1 for a,b,c,d in edges if n in (c,d)))
        #        for n in h.nodes()]
        #node_size = [300 * (d/max(degrees)) for d in degrees]

        hist = net.get_layer(to_layer).activ_history
        prefix_length = len(to_layer) + 1
        activ_counts = [hist.get(n[prefix_length:], 0) for n in h.nodes()]
        max_count = max(activ_counts)
        node_size = [300 * (c/max_count) for c in activ_counts]

        if labels:
            nx.draw_networkx_labels(h, poss, font_size=8)
        nx.draw_networkx_nodes(h, poss, node_color=node_colors, node_size=node_size)
        nx.draw_networkx_edges(h, poss, alpha=1.0, width=0.1)

        print()
        print(to_layer, list(sorted(hist.items(), reverse=True, key = lambda x: x[1])))
        print()

    # Draw inter-layer connections
    for to_layer in layers:
        e = [(c,d) for a,b,c,d in edges if a != b and b == to_layer]
        if len(e) > 0:
            g.add_edges_from(e)
    for to_layer in layers:
        e = [(c,d) for a,b,c,d in edges if a != b and b == to_layer]
        if len(e) > 0:
            nx.draw_networkx_edges(g, poss,
                edge_color=color_map[to_layer], alpha=0.25, edgelist=e)

    plt.show()

def draw_object_network(net, inc_classes=None, labels=True, skip_graph=False):

    # Get ctx associations
    rev_ctx_assoc = {}
    for k,v in net.get_connection("mem_ctx", "lex", "hetero").all_mappings.mappings.items():
        rev_ctx_assoc[v.get_symbol()] = k[0][1]

    # Get namespaces
    namespaces = {}
    var_counts = {}
    mem_bind = net.get_connection("mem", "bind", "hetero")

    hist = mem_bind.activ_history
    for (x,c),y in mem_bind.all_mappings.mappings.items():
        from_state = State.from_tuple(x)
        to_ctx = State.from_tuple(c)
        val = mem_bind.lookup(from_state, to_ctx).get_symbol()

        lex_ctx = rev_ctx_assoc[to_ctx.get_symbol()]

        binding = (lex_ctx, val)
        if x[1] not in namespaces:
            namespaces[x[1]] = [binding]
        else:
            namespaces[x[1]].append(binding)

        pair = (x[1], val)
        var_counts[pair] = hist.get(pair, 0)

    # Get mem objects
    #   -> link to class name (lex)
    #   -> link to namespace
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

    hist = mem_layer.activ_history
    for (x,c),y in lex_mem.all_mappings.mappings.items():
        y_sym = y.get_symbol()
        if y_sym == "#OBJECT":
            obj = State(mem_layer, x[1])

            namespace = bind_mem.lookup(obj).get_symbol()

            class_obj = car(obj)
            class_name = car(class_obj)
            class_sym = lex_mem.lookup(class_name).get_symbol()
            count = hist.get(x[1], 0)

            if inc_classes is None or class_sym in inc_classes:
                objs[x[1]] = (class_sym, count, namespace)
        elif y_sym[0] == "#":
            if y_sym in non_obj:
                non_obj[y_sym].append(x[1])
            else:
                non_obj[y_sym] = [x[1]]
        else:
            syms.append(x[1])

    edges = []
    classes = {}
    num_props = 0
    for obj, (class_sym, count, namespace) in objs.items():
        if class_sym not in classes:
            classes[class_sym] = 1
        else:
            classes[class_sym] += 1
        for c,v in namespaces[namespace]:
            if (obj != v) and (v in objs):
                edges.append((obj,v))

        print(obj, class_sym, count, len(namespaces[namespace]))
        num_props += len(namespaces[namespace])

    print("Objects: ", len(objs))
    for cls in sorted(classes.keys()):
        print("  %s: %d" % (cls, classes[cls]))
    print("Properties: ", num_props)
    print("Non-objects:")
    for typ in sorted(non_obj.keys()):
        count = len(non_obj[typ])
        flashed_count = sum(1 for x in non_obj[typ] if "main" in x)
        print("  %s: %d (Flashed: %d)" % (typ, count, flashed_count))
    print("Symbols: ", len(syms))


    if skip_graph or (len(edges) == 0):
        return

    g = nx.DiGraph()

    # Edge weight: activation count for mem<-bind
    edge_widths = []
    for fs, ts in edges:
        pair = (objs[fs][2], ts)
        count = var_counts[pair]
        edge_widths.append(count)
        #g.add_edge(fs,ts,weight=count)
        g.add_edge(fs,ts)

    mean_edge = np.mean(edge_widths)
    std_edge = np.std(edge_widths)
    if std_edge == 0.:
        edge_widths = [2.5 for w in edge_widths]
    else:
        edge_widths = [2.5 * (w - mean_edge) / std_edge for w in edge_widths]
        edge_widths = [min(max(0, w), 5)+0.5 for w in edge_widths]

    # Node color : class
    colors = ["red", "blue", "green", "orange",
              "purple", "brown", "black", "yellow"]
    cmap = { cls : colors[i%len(colors)] for i,cls in enumerate(sorted(classes.keys())) }
    print("Color map:", cmap)
    node_colors = []
    for n in g.nodes():
        node_colors.append(cmap[objs[n][0]])

    # Node size : activation count
    activ_counts = [objs[n][1] for n in g.nodes()]
    max_count = max(activ_counts)
    node_size = [300 * (c/max_count) for c in activ_counts]

    # Keep timepoint nodes in a fixed position
    fixed = [n  for n in g.nodes()
        if n in objs and objs[n][0] == "Timepoint"]
    fixed = list(sorted(fixed, key = lambda x: int(x[1:])))

    pos = { n : (float(i)/len(fixed), 0)
        for i,n in enumerate(fixed) }


    #pos=nx.kamada_kawai_layout(g,weight=0.5)
    #pos=nx.fruchterman_reingold_layout(g)
    pos=nx.fruchterman_reingold_layout(g, fixed=fixed, pos=pos)

    if labels:
        nx.draw_networkx_labels(g, pos, font_size=8)
    nx.draw_networkx_nodes(g, pos, node_color=node_colors, node_size=node_size)
    nx.draw_networkx_edges(g, pos, alpha=1.0, width=edge_widths)

    plt.show()
