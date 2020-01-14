CXX = clang++
CXXFLAGS = -Wall -Wextra -std=c++17 -g
OBJ = sexp.o sexp_test.o
BIN = test

all: $(OBJ)

%.o : %.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -c -o $@ $<

test: $(OBJ)
	$(CXX) $(OBJ) -o test

clean:
	rm -rf test $(OBJ)
