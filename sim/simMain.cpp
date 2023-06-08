#include <bits/stdc++.h>
#include "obj_dir/VCpu.h"
#include "obj_dir/VCpu___024root.h"
#include <verilated.h>
#include <verilated_vcd_c.h>

size_t getFileLength(std::ifstream &ifs) {
    const size_t begin = ifs.tellg();
    ifs.seekg(0, std::ios::end);
    const size_t end = ifs.tellg();
    const size_t fsize = end - begin;
    ifs.seekg(0, std::ios::beg);
    return fsize;
}

class Device {
public:
    uint64_t startAddr;
    uint64_t endAddr;
    union {
        uint8_t *data8;
        uint64_t *data64;
    };
    Device(uint64_t addr, size_t len) {
        if(len > 0) {
            data8 = new uint8_t[len];
            memset(data8, 0, len);
        }
        else {
            data8 = nullptr;
        }
        startAddr = addr;
        endAddr = addr + len - 1;
    }
    ~Device() {
        startAddr = 0;
        endAddr = 0;
        if(data8 != nullptr) delete[] data8;
        data8 = nullptr;
    }

    /*
        @return: pair(value, errcode)
    */
    virtual std::pair<uint64_t, uint64_t> access(uint64_t addr, uint64_t wdata, bool we) {
        assert(startAddr <= addr && addr <= endAddr);
        assert(data64 != nullptr);
        if(addr & 0x7) {
            printf("Misalignment: 0x%016lX\n", addr);
            return std::make_pair(0, 1);
        }
        if(we) {
            data64[(addr - startAddr) / sizeof(uint64_t)] = wdata;
        }
        return std::make_pair(data64[(addr - startAddr) / sizeof(uint64_t)], 0);
    }
    virtual void loadFromFile(const char *file) {
        std::ifstream fs(file, std::ios::in | std::ios::binary);
        size_t len = getFileLength(fs);
        assert(len <= (endAddr + 1 - startAddr));
        assert(data8 != nullptr);
        if(!fs) {
            std::cout << "Cannot open " << file << std::endl;
            fs.close();
            return;
        }
        fs.read(reinterpret_cast<char *>(data8), len);
        fs.close();
    }
};

class Rom: public Device {
public:
    std::pair<uint64_t, uint64_t> access(uint64_t addr, uint64_t wdata, bool we) {
        return Device::access(addr, 0, false);
    }
};

class Serial: public Device {
public:
    std::pair<uint64_t, uint64_t> access(uint64_t addr, uint64_t wdata, bool we) {
        if(we) {
            std::cerr.put(static_cast<char>(wdata));
        }
        return std::make_pair(0, 0);
    }
    Serial(uint64_t addr): Device(addr, 8) {}
    virtual void loadFromFile(const char *file) {
        assert(0);
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
    std::pair<uint64_t, uint64_t> access(uint64_t addr, uint64_t wdata, bool we) {
        for(auto i : devices) {
            if(i->startAddr <= addr && addr <= i->endAddr) {
                return i->access(addr, wdata, we);
            }
        }
        printf("No device matched: %016lX\n", addr);
        assert(0);
        return std::make_pair(0, 2);
    }
} mem;

int main(int argc, char **argv) {

    VCpu *dut;dut = new VCpu;

    Device ram(0x80000000ul, 0x20000000ul);
    Serial serial(0xA0000048ul);
    Device simEnd(0xB0000000ul, 8);

    uint64_t simTime = 0;

    VerilatedVcdC *tracer = new VerilatedVcdC;

    mem.add(&ram);
    mem.add(&serial);
    mem.add(&simEnd);

    ram.loadFromFile("sim/Helloworld.bin");

    Verilated::commandArgs(argc, argv);
    Verilated::traceEverOn(true);
    dut->trace(tracer, 5);
    tracer->open("sim/wave.vcd");

    dut->clock = 0;
    dut->reset = 0;
    dut->eval();
    tracer->dump(simTime++);

    dut->clock = 0;
    dut->reset = 1;
    dut->eval();
    tracer->dump(simTime++);

    dut->clock = 1;
    dut->eval();
    tracer->dump(simTime++);

    dut->clock = 0;
    dut->reset = 0;
    dut->eval();

    // while(!simEnd.data) {
    for(int i = 0; (!simEnd.data64[0]); i++) {

        printf("PC = %016lX\n", dut->io_pc);

        dut->io_imem_rdata = mem.access(dut->io_imem_addr, 0, false).first;
        tracer->dump(simTime++);
        dut->clock = 1;
        dut->eval();
        tracer->dump(simTime++);

        printf("inst = %08X\n", dut->io_inst);

        if(dut->io_err) {
            printf("Instruction not implemented: %08X\n", dut->io_inst);
            break;
        }

        dut->io_dmem_rdata = (dut->io_dmem_en) ? mem.access(dut->io_dmem_addr, dut->io_dmem_wdata, dut->io_dmem_we).first : 0;
        dut->clock = 0;
        dut->eval();
    }

    std::cout << "Simulation ended with simEnd = " << simEnd.data64[0] << std::endl;

    tracer->close();
    delete dut;

    return 0;

}