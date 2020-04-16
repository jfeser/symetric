CXX = clang++-9
CXXFLAGS = -Wall -Wextra -std=c++17 -g # -fsanitize=address \
					 # -fsanitize=undefined -fno-omit-frame-pointer
LDFLAGS = -fsanitize=address -fsanitize=undefined
OBJ = sexp.o sexp_test.o
BIN = test

all: $(OBJ)

%.o : %.cpp
	$(CXX) $(CXXFLAGS) $(INCLUDE) -c -o $@ $<

test: $(OBJ)
	$(CXX) $(LDFLAGS) $(OBJ) -o test

clean:
	rm -rf test $(OBJ)
