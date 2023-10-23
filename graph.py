import matplotlib.pyplot as plt
import re
import numpy as np
from scipy.spatial import ConvexHull

def interp(*axis_list):
    min_max_xs = [(min(axis[:,0]), max(axis[:,0])) for axis in axis_list]

    new_axis_xs = [np.linspace(min_x, max_x, 100) for min_x, max_x in min_max_xs]
    new_axis_ys = [np.interp(new_x_axis, axis[:,0], axis[:,1]) for axis, new_x_axis in zip(axis_list, new_axis_xs)]

    midx = [np.mean([new_axis_xs[axis_idx][i] for axis_idx in range(len(axis_list))]) for i in range(100)]
    midy = [np.mean([new_axis_ys[axis_idx][i] for axis_idx in range(len(axis_list))]) for i in range(100)]

    return (midx, midy)

def file_to_graph(fins, fout, cap=0):
    rdatasets = []
    sdatasets = []
    colors = { "R": "blue", "S": "orange" }
    for (name, fin) in fins:
        f = open(fin)

        content = f.read()

        [start] = re.findall(r"\?\? Start at time (\d+\.\d+)", content)
        start = float(start)

        results = (re.findall(r"!! Found EV of (\d+) at time (\d+\.\d+)", content))
        results = [(float(t)-start, int(ev)) for (ev,t) in results]

        xs, ys = unzip(results)
        if name == "R":
            rdatasets.append(np.array(results))
        elif name == "S":
            sdatasets.append(np.array(results))

    fig, ax = plt.subplots()
    fig.set_size_inches(5, 4)
    ax.grid()
    ax.set_xlim(0, 3600)
    ax.set(xlabel='time (s)', ylabel='value (eth)')

    for arr in rdatasets:
        ax.plot(arr[:,0], arr[:,1], label="R", color=colors["R"])

    for arr in sdatasets:
        ax.plot(arr[:,0], arr[:,1], label="S", color=colors["S"])
    
    # rs1 = np.vstack(rdatasets)
    # rhull = ConvexHull(rs1)
    # plt.fill(rs1[rhull.vertices,0], rs1[rhull.vertices,1], color = colors["R"], alpha=0.3)
    # rmidx, rmidy = interp(*rdatasets)
    # plt.plot(rmidx, rmidy)

    # ss1 = np.vstack(sdatasets)
    # shull = ConvexHull(ss1)
    # plt.fill(ss1[shull.vertices,0], ss1[shull.vertices,1], color = colors["S"], alpha=0.3)
    # smidx, smidy = interp(*sdatasets)
    # plt.plot(smidx, smidy)

    fig.legend(loc=2, ncol=1, borderaxespad=6)
    fig.savefig(fout)

def unzip(lrs):
    ls = [l for (l,_) in lrs]
    rs = [r for (_,r) in lrs]
    return (ls, rs)


# file_to_graph([("S", "bw_1_log"), ("R", "bw_naive_1_log")], "bw")
file_to_graph([("S", "cp_1b_log"), ("R", "cp_naive_1b_log")], "cp")
file_to_graph([("S", "hl_1_log"), ("S", "hl_2_log"), ("S", "hl_3_log"), ("R", "hl_naive_1_log")], "hl")
