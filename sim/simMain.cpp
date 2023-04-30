#include <bits/stdc++.h>
#include "obj_dir/VCpu.h"
#include "obj_dir/VCpu___024root.h"
#include <verilated.h>
#include <verilated_vcd_c.h>

VCpu *dut;

class Device {
public:
    uint64_t startAddr;
    uint64_t endAddr;
    virtual uint64_t access(uint64_t addr, uint64_t wdata, bool we) = 0;
};

class Rom: public Device {
public:
    uint64_t *data;
    uint64_t access(uint64_t addr, uint64_t wdata, bool we) {
        assert(startAddr <= addr && addr <= endAddr);
        return data[(addr - startAddr) / sizeof(uint64_t)];
    }
    Rom(const char *file, size_t len, uint64_t begin) {
        std::ifstream fs(file, std::ios::in | std::ios::binary);
        assert(!(begin & 0x7));
        startAddr = begin;
        endAddr = begin + len * sizeof(uint64_t) - 1;
        data = new uint64_t[len];
        if(!fs) {
            std::cout << "[Rom] Can't open " << file << ". Initialized with 0." << std::endl;
            memset(data, 0, len * sizeof(uint64_t));
            fs.close();
            return;
        }
        fs.read(reinterpret_cast<char *>(data), len * sizeof(uint64_t));
        fs.close();
    }
    ~Rom() {
        delete[] data;
    }
};

class MMReg: public Device {
public:
    uint64_t data;
    uint64_t access(uint64_t addr, uint64_t wdata, bool we) {
        assert(startAddr == addr);
        if(we) {
            data = wdata;
        }
        return data;
    }
    MMReg(uint64_t addr, uint64_t initVal = 0) {
        data = initVal;
        startAddr = addr;
        endAddr = addr;
    }
};

class MemoryMap {
private:
    std::vector<Device *> devices;
public:
    MemoryMap() = default;
    void add(Device *device) {
        devices.push_back(device);
    }
    uint64_t access(uint64_t addr, uint64_t wdata, bool we) {
        for(auto i : devices) {
            if(i->startAddr <= addr && addr <= i->endAddr) {
                return i->access(addr, wdata, we);
            }
        }
        printf("No device matched: %016lX\n", addr);
        assert(0);
        return 0;
    }
} mem;

int main(int argc, char **argv) {

    MMReg simEnd(0x80000800);
    MMReg serial(0xA0000048);

    mem.add(new Rom("sim/rom.bin", 35, 0x80000000ul)); // related to simplecpu/
    mem.add(&simEnd);
    mem.add(&serial);

    Verilated::commandArgs(argc, argv);
    dut = new VCpu;
    Verilated::traceEverOn(true);
    VerilatedVcdC *tracer = new VerilatedVcdC;
    dut->trace(tracer, 5);
    tracer->open("sim/wave.vcd");

    uint64_t simTime = 0;

    dut->clock = 0;
    dut->reset = 0;
    dut->eval();
    tracer->dump(simTime++);

    dut->reset = 1;
    dut->clock = 1;
    dut->eval();
    tracer->dump(simTime++);

    dut->clock = 0;
    dut->eval();
    tracer->dump(simTime++);

    dut->reset = 0;

    // while(!simEnd.data) {
    for(int i = 0; (!simEnd.data); i++) {

        printf("PC = %016lX\n", dut->io_pc);

        dut->io_imem_rdata = mem.access(dut->io_pc, 0, false);
        dut->clock = 1;
        dut->eval();
        tracer->dump(simTime++);

        if(dut->io_err) {
            printf("Instruction not implemented: %016lX \t%08X\n", dut->io_pc, dut->io_inst);
            break;
        }

        dut->io_dmem_rdata = (dut->io_dmem_en) ? mem.access(dut->io_dmem_addr, dut->io_dmem_wdata, dut->io_dmem_we) : 0;
        dut->clock = 0;
        dut->eval();
        tracer->dump(simTime++);

        if(serial.data != 0) {
            std::cerr.put(static_cast<char>(serial.data));
            serial.data = 0;
        }
    }

    std::cout << "Simulation ended with simEnd = " << simEnd.data << std::endl;

    tracer->close();
    delete dut;
    exit(EXIT_SUCCESS);
    // return 0;

}