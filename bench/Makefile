TIME=timeout --machine-readable --
CXXFLAGS = -Wall -Wextra -std=c++17 -g
OUTPUTS := $(patsubst %.sexp,%.time,$(wildcard %.sexp))

synth:
	$(CXX) $(CXXFLAGS) *.cpp -o synth

%.time: %.sexp synth
	$(TIME) ./synth < $< > $*.json

run: $(addsuffix .time, $(basename $(wildcard *.sexp)))
