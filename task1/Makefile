CXX     = g++
CFLAGS  = --std=c++20
SOURCES = $(*.cpp)
HEADERS = $(calculator.h)

calc: $(HEADERS) $(SOURCES)
	$(CXX) $(CFLAGS) main.cpp calculator.cpp -o $@

test: calc CMakeLists.txt
	cmake -S . -B build && cmake --build build && cd build && ctest

clean:
	rm -rf build calc
