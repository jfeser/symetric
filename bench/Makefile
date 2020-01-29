TIMEOUT=timeout_osx.exe
TIMEOUTFLAGS=--machine-readable
CXX=clang++
CXXFLAGS = -Wall -Wextra -std=c++17 -g
OUTPUTS := $(patsubst %.sexp,%.time,$(wildcard %.sexp))

synth:
	$(CXX) $(CXXFLAGS) *.cpp -o synth

%.time: %.sexp synth
	$(TIMEOUT) $(TIMEOUTFLAGS) -- ./synth < $< > $@

run: $(addsuffix .time, $(basename $(wildcard *.sexp)))
