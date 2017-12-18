// exercise 7.7: given map(y)(id) == y , it's free theorm that
// map(map(y)(g))(f) == map(y)(f compose g), aka "map fusion".

map(map(y)(g))(f) == map(y)(f•g)
map(map(unit(x))(g))(f) == map(unit(x))(f•g)
map(unit(g(x)))(f) == unit(f•g(x))
unit(f(g(x))) == unit(f•g(x))
f(g(x)) == f•g(x)

// also --

map(map(y)(g))(id) == map(y)(g)
map(y)(g) == map(y)(g)

// also --

map(map(y)(id))(f) == map(y)(f)
map(y)(f) == map(y)(f)

// exercise 7.8: various ExecutorServices to make sure
// `fork(x) == x` holds true for all choices of x and
// ExecutorServices.
