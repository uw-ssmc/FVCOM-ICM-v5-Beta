# FVCOM 4.3 Water Quality Model (WQM)

This repository contains a Fortran-based FVCOM 4.3 water quality model implementation and related build inputs.

## Repository layout

- `wqmsrc/`: primary model source tree and build files.
- `inputs/`: model input data (case-dependent runtime files).
- `METIS_source/`: METIS library source used for multiprocessor builds.
- `WQM_welcome.nml`: top-level namelist/configuration entry point.

## Build

The active build configuration is in `wqmsrc/makefile`.

Current default executable name:

- `wqm_txc_v4`

Typical build sequence:

```bash
cd wqmsrc
make clean
make
```

## Toolchain and dependencies

The current makefile is configured for:

- `mpif90` (MPI-enabled Fortran build)
- NetCDF Fortran library (`-lnetcdff`)
- METIS linkage (`-L../METIS_source -lmetis`)

Update compiler paths, NetCDF include/library paths, and compile flags in `wqmsrc/makefile` for your target system.

## Source organization

Model code is split across many Fortran modules and translated `.f90` files generated from `.F` preprocessing rules during compilation. Key areas include:

- Core model/state modules: `mod_*.F`
- Numerical kernels and transport: `adv_wqm.F`, `vdif_wqm.F`, `tvds.F`, `vertvl.F`
- I/O and configuration: `mod_ncd.F`, `ncdio.F`, `wqm_inputs.F`, `mod_input.F`

## Running

Runtime behavior is controlled by namelist and input files. A common workflow is:

```bash
cd wqmsrc
./wqm_txc_v4
```

Ensure all required runtime inputs are available in expected paths before execution.

## Notes

- This repository contains generated build artifacts (`*.o`, `*.mod`, and generated `*.f90` files in some workflows).
- Use `make clean` to remove local build products before rebuilding.
