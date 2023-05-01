#include <bits/stdc++.h>
#include "obj_dir/VCpu.h"
#include "obj_dir/VCpu___024root.h"
#include <verilated.h>
#include <verilated_vcd_c.h>

VCpu *dut;

size_t getFileLength(std::ifstream &ifs) {
    const size_t begin = ifs.tellg();
    ifs.seekg(0, std::ios::end);
    const size_t end = ifs.tellg();
    const size_t fsize = end-begin;
    ifs.seekg(0, std::ios::beg);
    return fsize;
}

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
        size_t fileLen = getFileLength(fs);
        uint64_t romLen = (len + 7) / sizeof(uint64_t);
        assert(!(begin & 0x7));
        startAddr = begin;
        endAddr = begin + romLen * sizeof(uint64_t) - 1;
        data = new uint64_t[romLen];
        memset(data, 0, romLen * sizeof(uint64_t));
        if(!fs) {
            std::cout << "[Rom] Can't open " << file << std::endl;
            fs.close();
            return;
        }
        fs.read(reinterpret_cast<char *>(data), fileLen);
        fs.close();
    }
    ~Rom() {
        delete[] data;
    }
};

class Ram: public Device {
public:
    uint64_t *data;
    uint64_t access(uint64_t addr, uint64_t wdata, bool we) {
        assert(startAddr <= addr && addr <= endAddr);
        if(we) {
            data[(addr - startAddr) / sizeof(uint64_t)] = wdata;
        }
        return data[(addr - startAddr) / sizeof(uint64_t)];
    }
    Ram(size_t len, uint64_t begin) {
        uint64_t ramLen = (len + 7) / sizeof(uint64_t);
        assert(!(begin & 0x7));
        startAddr = begin;
        endAddr = begin + ramLen * sizeof(uint64_t) - 1;
        data = new uint64_t[ramLen];
        memset(data, 0, ramLen * sizeof(uint64_t));
    }
    void load(const char *file) {
        std::ifstream fs(file, std::ios::in | std::ios::binary);
        size_t len = getFileLength(fs);
        assert(len <= (endAddr + 1 - startAddr));
        if(!fs) {
            std::cout << "[Rom] Can't open " << file << std::endl;
            fs.close();
            return;
        }
        fs.read(reinterpret_cast<char *>(data), len);
        fs.close();
    }
    ~Ram() {
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

    MMReg simEnd(0xB0000000);
    MMReg serial(0xA0000048);
    Ram ram(0x20000000ul, 0x80000000ul);

    mem.add(&ram);
    mem.add(&simEnd);
    mem.add(&serial);

    ram.load("sim/Helloworld.bin");

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

        printf("inst = %08X\n", dut->io_inst);

        if(dut->io_err) {
            printf("Instruction not implemented: %08X\n", dut->io_inst);
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