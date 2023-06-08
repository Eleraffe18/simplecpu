.PHONY: sv sim clean runsim
sv:
	@sbt "runMain simplecpu.Main --target-dir build --split-verilog"

sim: sv
	@cd sim && verilator --trace --top-module Cpu --cc ../build/*.sv --exe simMain.cpp -Wno-WIDTHEXPAND --x-assign unique --x-initial unique
	@make -C sim/obj_dir -f VCpu.mk VCpu

runsim:
	@./sim/obj_dir/VCpu  +verilator+rand+reset+2

clean:
	-@rm -rf build
	-@rm -rf sim/obj_dir
	-@rm -rf sim/build
	-@rm sim/*.vcd