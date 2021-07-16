/* -*- mode: c++; coding: utf-8; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4; show-trailing-whitespace: t -*- vim: set fenc=utf-8 ft=cpp et sw=4 ts=4 sts=4: */

#include "ABL.hpp"
#include <iostream>

int main(int argc, char** argv)
{
    // Initial and forcing values
    const double albedo = 0.95;
    const double ug     = 10.;
    const double vg     = 10.;
    const double slon   = 180.;
    const double semis  = 0.95;
    const double rlat   = 85.;
    const double z0     = 2e-2;
    const double taur   = 0.10;
    const double p0     = 1013e2;
    const double q0     = 10e-3;
    const double t0     = 273.15;
    const double ds     = 30;
    const double ha     = 3.141592654;
    const double jd     = 1;
    const int    nj     = 121;

    // Prognostic variables
    std::vector<double> dedzm;
    std::vector<double> dedzt;
    std::vector<double> zm;
    std::vector<double> zt;
    std::vector<double> u;
    std::vector<double> v;
    std::vector<double> t;
    std::vector<double> q;
    std::vector<double> qi;
    std::vector<double> e;
    std::vector<double> ep;
    std::vector<double> uw;
    std::vector<double> vw;
    std::vector<double> wt;
    std::vector<double> wq;
    std::vector<double> wqi;
    std::vector<double> km;
    std::vector<double> kh;
    double ustar;

    // Initialise
    ABL::init(albedo, ug, vg, slon, semis, rlat, z0, taur, p0, q0, t0, nj,
            dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, ustar);

    // Run
    ABL::step(albedo, ug, vg, slon, semis, rlat, z0, taur, p0, ds, ha, jd,
            dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, ustar);

}
