/* -*- mode: c++; coding: utf-8; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4; show-trailing-whitespace: t -*- vim: set fenc=utf-8 ft=cpp et sw=4 ts=4 sts=4: */

#ifndef __ABL_HPP
#define __ABL_HPP 1

#include <vector>
#include <cassert>

namespace ABL
{

    void init(
            // Inputs:
            const double albedo, const double ug, const double vg, const double slon, const double semis, const double rlat,
            const double z0, const double taur, const double p0, const double q0, const double t0,
            const int nj,
            // Outputs:
            std::vector<double> & dedzm, std::vector<double> & dedzt, std::vector<double> & zm, std::vector<double> & zt,
            std::vector<double> & u, std::vector<double> & v, std::vector<double> & t, std::vector<double> & q,
            std::vector<double> & qi, std::vector<double> & e, std::vector<double> & ep, std::vector<double> & uw,
            std::vector<double> & vw, std::vector<double> & wt, std::vector<double> & wq, std::vector<double> & wqi,
            std::vector<double> & km, std::vector<double> & kh, double & ustar);

    void step(
            // Inputs:
            const double albedo, const double ug, const double vg, const double slon, const double semis, const double rlat,
            const double z0, const double taur, const double p0,const double ds, const double ha, const double jd,
            // Outputs:
            std::vector<double> & dedzm, std::vector<double> & dedzt, std::vector<double> & zm, std::vector<double> & zt,
            std::vector<double> & u, std::vector<double> & v, std::vector<double> & t, std::vector<double> & q,
            std::vector<double> & qi, std::vector<double> & e, std::vector<double> & ep, std::vector<double> & uw,
            std::vector<double> & vw, std::vector<double> & wt, std::vector<double> & wq, std::vector<double> & wqi,
            std::vector<double> & km, std::vector<double> & kh, double & ustar);
}
#endif
