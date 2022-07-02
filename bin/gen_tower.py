def bridge(w, h):
    return f"(for {w} (for {h} v (r 2) v (l 2)) h (r 3))"


def brick(w, h):
    return f"(for {h} (embed (for {w} h (r 3))) (embed (r 1) (for {w} h (r 3))))"


twoArches = [
    f"""((for {n1} v) (r 2) (for {n1} v) (l 2) h (r {dist}) (for {n2} v) (r 2) (for {n2} v) (l 2) h)"""
    for (n1, n2, dist) in [
        (n1, n2, dist) for n1 in range(2, 5) for n2 in range(2, 4) for dist in [6, 8]
    ]
]

brick_next_to_bridges = [
    f"({brick(w1, h1)} (r 16) {bridge(w2, h2)})"
    for w1, h1, w2, h2 in [(4, 8, 5, 8), (4, 6, 5, 8), (4, 6, 5, 6), (4, 8, 5, 6)]
]

bridges_next_to_bridges = [
    f"({bridge(w1, h1)} (r {d}) {bridge(w2, h2)})"
    for w1, h1, w2, h2, d in [
        (4, 8, 4, 8, 6),
        (4, 8, 4, 8, 8),
        (4, 6, 4, 8, 6),
        (3, 8, 4, 8, 6),
        (3, 8, 4, 6, 6),
        (4, 8, 4, 6, 6),
    ]
]

bridges_on_bridges = [
    f"((embed {bridge(w1, h1)}) {bridge(w2, h2)})"
    for w1, h1, w2, h2 in [
        (5, 6, 3, 4),
        (5, 4, 3, 4),
        (5, 6, 3, 2),
        (5, 6, 4, 4),
        (5, 4, 2, 4),
        (4, 6, 3, 4),
        (4, 8, 3, 6),
    ]
]

compositions = [
    """((for %d (embed (for %d h (r 3))) (embed (r 1) (for %d h (r 3)))) (r 1) (for %d (for %d v (r 2) v (l 2)) h (r 3)))"""
    % (w1, w2, w2, b1, b2)
    for b1, b2, w1, w2 in [(5, 2, 4, 5)]
] + [
    """((for %d (embed (for %d h (r 3))) (embed (r 1) (for %d h (r 3)))) (r 3) %s (r 2) %s (l 2) h)"""
    % (w1, w2, w2, "v " * t, "v " * t)
    for t, w1, w2 in [(3, 1, 3), (5, 2, 3), (4, 2, 4), (4, 2, 3)]
]  # can't be (4,1,3)

everything = (
    twoArches
    + brick_next_to_bridges
    + bridges_next_to_bridges
    + bridges_on_bridges
    + compositions
)

for i, bench in enumerate(everything):
    with open(f"bench/tower/test{i}.sexp", "w") as f:
        f.write(bench)
