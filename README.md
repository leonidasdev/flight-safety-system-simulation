# Flight Safety System Simulation

A real-time flight safety system simulation implemented in Ada for the SPARC/ERC32 architecture. This project demonstrates a safety-critical embedded system with concurrent tasks managing aircraft control, obstacle detection, and pilot interface.

## Overview

This simulation implements a Flight Safety System (FSS) that manages multiple aspects of aircraft control including pitch, roll, speed, altitude, and obstacle detection. The system operates in both automatic and manual modes, with real-time task scheduling and priority-based resource management.

### Team Members
- Leonardo Chen
- Pablo Gil
- Mario Lorenzo
- Zixin Zheng

## Features

### Implemented Tasks

The system consists of several concurrent tasks with different priorities:

1. **Task_Control_Cabeceo_Altitud** (Priority: 11)
   - Controls aircraft pitch and altitude
   - Highest priority for critical flight control

2. **Task_Control_Alabeo** (Priority: 10)
   - Manages aircraft roll control
   - Ensures stable lateral movement

3. **Task_Control_Velocidad** (Priority: 8)
   - Controls aircraft speed
   - Manages velocity adjustments

4. **Task_Deteccion_Obstaculos** (Priority: 13)
   - Detects obstacles in the flight path
   - Critical safety feature with highest priority

5. **Task_Display** (Priority: 5)
   - Manages display output
   - Shows system status and flight information

6. **Task_Mode**
   - Controls automatic/manual mode switching
   - Handles pilot input for mode selection

### Protected Objects

The system uses several protected objects for safe concurrent data access:

- **Pitch_Roll_Command**: Synchronizes joystick data input
- **Pitch**: Manages aircraft pitch control
- **Roll**: Controls aircraft roll
- **Speed**: Manages current velocity
- **Status_Record**: Shared status information registry
- **Selected_Mode**: Handles automatic/manual mode selection
- **Interruption_Handler**: Manages button interrupt handling

## System Architecture

### Input Devices
- **Obstacle Distance Sensor**: Range 0-9000 units
- **Daylight Intensity Sensor**: Range 0-1023 units
- **Joystick**: Pilot control input

### Aircraft Control
- **Altitude**: 0-15,000 meters
- **Speed**: 0-1,100 km/h (0-10,230 units)
- **Pitch**: -90° to +90°
- **Roll**: -90° to +90°

### Workload
The system uses a synthetic computational workload based on the Whetstone benchmark (Small_Whetstone) to simulate real-world processing demands.

## Building the Project

### Requirements
- SPARC ORK Ada compiler (`sparc-ork-gnatmake`)
- SPARC ORK binutils (`sparc-ork-size`, `sparc-ork-nm`)
- TSIM ERC32 simulator
- GNAT Ada toolchain

### Build Commands

```bash
# Build the project
make all

# Clean build artifacts
make clean

# Run the simulation
make run
```

### Build Process

The Makefile configures the following:
- **Simulator**: TSIM-ERC32
- **Main Program**: `main.adb`
- **Target**: SPARC/Cypress architecture
- **Compilation Flags**: Debug mode (`-g`)
- **Link Specs**: ORK specifications with Cypress CPU support

Build outputs include:
- Executable binary
- Symbol table (`.nm` file)
- Memory map (`.map` file)
- Size information

## Project Structure

```
flight-safety-system-simulation-main/
├── main.adb                           # Main entry point
├── fss.ads                            # FSS package specification
├── fss.adb                            # FSS package implementation
├── devicesfss_v1.ads                  # Device interface specifications
├── devicesfss_v1.adb                  # Device interface implementation
├── button_interrupt.ads               # Button interrupt handler spec
├── button_interrupt.adb               # Button interrupt handler impl
├── force_external_interrupt_2.adb     # External interrupt generator
├── tools.ads                          # Utility functions specification
├── tools.adb                          # Utility functions implementation
├── workload.ads                       # Whetstone workload specification
├── workload.adb                       # Whetstone workload implementation
├── testing_1.ads                      # Test configuration
├── testing_1_original.ads             # Original test configuration
├── gnat.adc                           # GNAT configuration
├── Makefile                           # Build configuration
├── VolcarTTY1.sh                      # TTY1 output script
└── VolcarTTY2.sh                      # TTY2 output script
```

## Real-Time Scheduling

The system implements the **Priority Ceiling Protocol** for resource management:
- Protected objects have ceiling priorities matching the highest priority of accessing tasks
- Prevents priority inversion
- Ensures deterministic behavior in real-time operations

## Safety Features

- **Obstacle Detection**: High-priority task for collision avoidance
- **Concurrent Control**: Multiple independent control tasks
- **Protected Data Access**: Thread-safe data structures
- **Mode Switching**: Safe transitions between automatic and manual modes
- **Interrupt Handling**: External interrupt support for button inputs

## Running the Simulation

1. Build the project using `make all`
2. The compiled binary targets the SPARC/ERC32 architecture
3. Run using `make run` or directly with the TSIM simulator:
   ```bash
   tsim-erc32 main
   ```

## Notes

- The button interrupt package should only be activated when properly programmed
- Debug mode is enabled by default; remove `-g` flag for optimization
- The system is designed for embedded real-time environments
- All timing is based on Ada.Real_Time for precise control

## License

This project is licensed under the MIT License. See the [LICENSE](./LICENSE) file for details.

## Target Platform

**Processor**: SPARC ERC32  
**Simulator**: TSIM-ERC32  
**Language**: Ada (using GNAT)  
**Real-Time Profile**: Ravenscar-compatible scheduling
