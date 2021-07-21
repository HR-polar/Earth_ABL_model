/* -*- mode: c++; coding: utf-8; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4; show-trailing-whitespace: t -*- vim: set fenc=utf-8 ft=cpp et sw=4 ts=4 sts=4: */

/**
 * @file   ABLextern.hpp
 * @author Einar Olason <einar.olason@nersc.no>
 * @date   Mon 19 Jul 2021 13:49:47 CEST
 */

#ifndef __ABLextern_HPP
#define __ABLextern_HPP 1

extern "C"
{
    // NB: The function names must be lower case - otherwise the linker has problems because Fortran is case insensitive.
    void initabl_c(
            // inputs:
            const double *, // albedo
            const double *, // ug
            const double *, // vg
            const double *, // slon
            const double *, // semis
            const double *, // rlat
            const double *, // z0
            const double *, // taur
            const double *, // p0
            const double *, // q0
            const double *, // t0
            const int    *, // nj
            // outputs:
            double *,  // dedzm
            double *,  // dedzt
            double *,  // zm
            double *,  // zt
            double *,  // u
            double *,  // v
            double *,  // t
            double *,  // q
            double *,  // qi
            double *,  // e
            double *,  // ep
            double *,  // uw
            double *,  // vw
            double *,  // wt
            double *,  // wq
            double *,  // wqi
            double *,  // km
            double *,  // kh
            double *); // ustar

    void stepabl_c(
            // inputs:
            const double *, // albedo
            const double *, // ug
            const double *, // vg
            const double *, // slon
            const double *, // semis
            const double *, // rlat
            const double *, // z0
            const double *, // taur
            const double *, // p0
            const double *, // ds
            const double *, // ha
            const double *, // jd
            const int    *, // nj
            // outputs:
            double *,  // dedzm
            double *,  // dedzt
            double *,  // zm
            double *,  // zt
            double *,  // u
            double *,  // v
            double *,  // t
            double *,  // q
            double *,  // qi
            double *,  // e
            double *,  // ep
            double *,  // uw
            double *,  // vw
            double *,  // wt
            double *,  // wq
            double *,  // wqi
            double *,  // km
            double *,  // kh
            double *); // ustar
}

#endif
