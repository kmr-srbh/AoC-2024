![aoc2024](media/aoc2024.jpg)
============

[Advent of Code 2024](https://adventofcode.com/2024) with Modern Fortran.

[![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)](https://github.com/topics/fortran)
[![Build Status](https://github.com/jacobwilliams/AoC-2024/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/AoC-2024/actions)

## Compiling

All the cases can be compiled and run using the [Fortran Package Manager](https://fpm.fortran-lang.org).

To build a conda environment using [pixi](https://pixi.sh/latest/switching_from/conda/#why-pixi) and activate it:

```
pixi init env
cd env
pixi add gfortran=13.2 fpm=0.10 ford=7.0 gfortran=13.2.0 python=3.13 graphviz=12.0 numpy==2.1 ipython==8.30
pixi shell
```

### to run individual cases:

```
fpm run --profile release problem_01
```

### to run them all:

```
fpm run --profile release --all
```

## Current status

<!-- ⭐☆ -->

Problem  | Stars  | Solution | Runtime
--       | --     | --       | --
[1](https://adventofcode.com/2024/day/1)  | ⭐⭐ | [problem_01.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_01.f90)  | 1 ms
[2](https://adventofcode.com/2024/day/2)  | ⭐⭐ | [problem_02.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_02.f90)  | 4 ms
[3](https://adventofcode.com/2024/day/3)  | ⭐⭐ | [problem_03.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_03.f90)  | 1 ms
[4](https://adventofcode.com/2024/day/4)  | ⭐⭐ | [problem_04.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_04.f90)  | 36 ms
[5](https://adventofcode.com/2024/day/5)  | ☆☆ | [problem_05.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_05.f90)  |
[6](https://adventofcode.com/2024/day/6)  | ☆☆ | [problem_06.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_06.f90)  |
[7](https://adventofcode.com/2024/day/7)  | ☆☆ | [problem_07.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_07.f90)  |
[8](https://adventofcode.com/2024/day/8)  | ☆☆ | [problem_08.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_08.f90)  |
[9](https://adventofcode.com/2024/day/9)  | ☆☆ | [problem_09.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_09.f90)  |
[10](https://adventofcode.com/2024/day/10)| ☆☆ | [problem_10.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_10.f90)  |
[11](https://adventofcode.com/2024/day/11)| ☆☆ | [problem_11.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_11.f90)  |
[12](https://adventofcode.com/2024/day/12)| ☆☆ | [problem_12.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_12.f90)  |
[13](https://adventofcode.com/2024/day/13)| ☆☆ | [problem_13.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_13.f90)  |
[14](https://adventofcode.com/2024/day/14)| ☆☆ | [problem_14.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_14.f90) |
[15](https://adventofcode.com/2024/day/15)| ☆☆ | [problem_15.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_15.f90) |
[16](https://adventofcode.com/2024/day/16)| ☆☆ | [problem_16.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_16.f90)  |
[17](https://adventofcode.com/2024/day/17)| ☆☆ | [problem_17.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_17.f90) |
[18](https://adventofcode.com/2024/day/18)| ☆☆ | [problem_18.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_18.f90) |
[19](https://adventofcode.com/2024/day/19)| ☆☆ | [problem_19.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_19.f90)  |
[20](https://adventofcode.com/2024/day/20)| ☆☆ | [problem_20.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_20.f90) |
[21](https://adventofcode.com/2024/day/21)| ☆☆ | [problem_21.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_21.f90) |
[22](https://adventofcode.com/2024/day/22)| ☆☆ | [problem_22.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_22.f90) |
[23](https://adventofcode.com/2024/day/23)| ☆☆ | [problem_23.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_23.f90) |
[24](https://adventofcode.com/2024/day/24)| ☆☆ | [problem_24.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_24.f90) |
[25](https://adventofcode.com/2024/day/25)| ☆☆ | [problem_25.f90](https://github.com/jacobwilliams/AoC-2024/blob/master/app/problem_25.f90) |

† With OpenMP enabled (i.e, add `--flag "-fopenmp"` to the FPM call).

## Previous Years

 * [AoC-2020](https://github.com/jacobwilliams/AoC-2020)
 * [AoC-2021](https://github.com/jacobwilliams/AoC-2021)
 * [AoC-2022](https://github.com/jacobwilliams/AoC-2022)
 * [AoC-2023](https://github.com/jacobwilliams/AoC-2023)
 * [AoC-2024](https://github.com/jacobwilliams/AoC-2024)

 ## Documentation

 * The API documentation for the current ```master``` branch can be found [here](https://jacobwilliams.github.io/AoC-2024/).  This is generated by processing the source files with [FORD](https://github.com/Fortran-FOSS-Programmers/ford).
