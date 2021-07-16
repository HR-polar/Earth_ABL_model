/* -*- mode: c++; coding: utf-8; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4; show-trailing-whitespace: t -*- vim: set fenc=utf-8 ft=cpp et sw=4 ts=4 sts=4: */

#include "ABL.hpp"

extern "C"
{
    void initabl_c(
            // inputs:
            const double *, //albedo
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

void ABL::init(
        // Inputs:
        const double albedo, const double ug, const double vg, const double slon, const double semis, const double rlat,
        const double z0, const double taur, const double p0, const double q0, const double t0,
        const int nj,
        // Outputs:
        std::vector<double> & dedzm, std::vector<double> & dedzt, std::vector<double> & zm, std::vector<double> & zt,
        std::vector<double> & u, std::vector<double> & v, std::vector<double> & t, std::vector<double> & q,
        std::vector<double> & qi, std::vector<double> & e, std::vector<double> & ep, std::vector<double> & uw,
        std::vector<double> & vw, std::vector<double> & wt, std::vector<double> & wq, std::vector<double> & wqi,
        std::vector<double> & km, std::vector<double> & kh, double & ustar)
{
    // This is init so we resize all the vectors
    dedzm.resize(nj);
    dedzt.resize(nj);
    zm.resize(nj);
    zt.resize(nj);
    u.resize(nj);
    v.resize(nj);
    t.resize(nj);
    q.resize(nj);
    qi.resize(nj);
    e.resize(nj);
    ep.resize(nj);
    uw.resize(nj);
    vw.resize(nj);
    wt.resize(nj);
    wq.resize(nj);
    wqi.resize(nj);
    km.resize(nj);
    kh.resize(nj);

    // Call the C/Fortran function
    // We always use the addres - even for the inputs - because Fortan passes by reference.
    initabl_c(&albedo, &ug, &vg, &slon, &semis, &rlat, &z0, &taur, &p0, &q0, &t0, &nj,
            &dedzm[0], &dedzt[0], &zm[0], &zt[0], &u[0], &v[0], &t[0], &q[0], &qi[0], &e[0], &ep[0], &uw[0], &vw[0], &wt[0],
            &wq[0], &wqi[0], &km[0], &kh[0], &ustar);
}

void ABL::step(
        // Inputs:
        const double albedo, const double ug, const double vg, const double slon, const double semis, const double rlat,
        const double z0, const double taur, const double p0,const double ds, const double ha, const double jd,
        // Outputs:
        std::vector<double> & dedzm, std::vector<double> & dedzt, std::vector<double> & zm, std::vector<double> & zt,
        std::vector<double> & u, std::vector<double> & v, std::vector<double> & t, std::vector<double> & q,
        std::vector<double> & qi, std::vector<double> & e, std::vector<double> & ep, std::vector<double> & uw,
        std::vector<double> & vw, std::vector<double> & wt, std::vector<double> & wq, std::vector<double> & wqi,
        std::vector<double> & km, std::vector<double> & kh, double & ustar)
{
    // This is step, so we assume the vectors are correctly sized
    const int nj = dedzm.size();
    assert ( nj == dedzt.size() );
    assert ( nj == zm.size() );
    assert ( nj == zt.size() );
    assert ( nj == u.size() );
    assert ( nj == v.size() );
    assert ( nj == t.size() );
    assert ( nj == q.size() );
    assert ( nj == qi.size() );
    assert ( nj == e.size() );
    assert ( nj == ep.size() );
    assert ( nj == uw.size() );
    assert ( nj == vw.size() );
    assert ( nj == wt.size() );
    assert ( nj == wq.size() );
    assert ( nj == wqi.size() );
    assert ( nj == km.size() );
    assert ( nj == kh.size() );

    // Call the C/Fortran function
    // We always use the addres - even for the inputs - because Fortan passes by reference.
    stepabl_c(&albedo, &ug, &vg, &slon, &semis, &rlat, &z0, &taur, &p0, &ds, &ha, &jd, &nj,
            &dedzm[0], &dedzt[0], &zm[0], &zt[0], &u[0], &v[0], &t[0], &q[0], &qi[0], &e[0], &ep[0], &uw[0], &vw[0], &wt[0],
            &wq[0], &wqi[0], &km[0], &kh[0], &ustar);
}

