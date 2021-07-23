/* -*- mode: c++; coding: utf-8; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4; show-trailing-whitespace: t -*- vim: set fenc=utf-8 ft=cpp et sw=4 ts=4 sts=4: */

#include "ABLextern.hpp"
#include <vector>
#include <iostream>

int main(int argc, char** argv)
{
    // Initial and forcing values - ABL
    const double albedo = 0.367;
    const double ug     = 5.;
    const double vg     = 0.;
    const double slon   = 226.;
    const double semis  = 0.96;
    const double rlat   = 29.;
    const double z0     = 0.001;
    const double taur   = 0.;
    const double p0     = 1015;
    const double q0     = 0.1;
    const double t0     = 300;
    const double ds     = 5;
    const double ha     = 3.141592654;
    const double jd     = 1;
    const int    nj     = 241;

    // Prognostic variables - ABL
    std::vector<double> dedzm(nj);
    std::vector<double> dedzt(nj);
    std::vector<double> zm(nj);
    std::vector<double> zt(nj);
    std::vector<double> u(nj);
    std::vector<double> v(nj);
    std::vector<double> t(nj);
    std::vector<double> q(nj);
    std::vector<double> qi(nj);
    std::vector<double> e(nj);
    std::vector<double> ep(nj);
    std::vector<double> uw(nj);
    std::vector<double> vw(nj);
    std::vector<double> wt(nj);
    std::vector<double> wq(nj);
    std::vector<double> wqi(nj);
    std::vector<double> km(nj);
    std::vector<double> kh(nj);
    std::vector<double> tld(nj);
    double ustar;

    // Soil model variables
    const int ni = 11;

    std::vector<double> dedzs(ni);
    std::vector<double> tsoil(ni);
    std::vector<double> zsoil(ni);
    double dzeta, gflux;

    // Initialise
    // Call the C/Fortran function
    // We always use the addres - even for the inputs - because Fortan passes by reference.
    initabl_c(&albedo, &ug, &vg, &slon, &semis, &rlat, &z0, &taur, &p0, &q0, &t0, &nj,
            &dedzm[0], &dedzt[0], &zm[0], &zt[0], &u[0], &v[0], &t[0], &q[0], &qi[0], &e[0], &ep[0], &uw[0], &vw[0], &wt[0],
            &wq[0], &wqi[0], &km[0], &kh[0], &ustar, &tld[0]);

    // Initialise the soil model
    std::cout << "Init done\n";
    subsoilt_c(&dedzs[0], &tsoil[0], &zsoil[0], &dzeta, &t[0], &z0, &ni);

    // Run
    // Call the C/Fortran function
    // We always use the addres - even for the inputs - because Fortan passes by reference.
    for ( int i=0; i<100; ++i)
    {
        // ABL
        stepabl_c(&albedo, &ug, &vg, &slon, &semis, &rlat, &z0, &taur, &p0, &ds, &ha, &jd, &nj,
                &dedzm[0], &dedzt[0], &zm[0], &zt[0], &u[0], &v[0], &t[0], &q[0], &qi[0], &e[0], &ep[0], &uw[0], &vw[0], &wt[0],
                &wq[0], &wqi[0], &km[0], &kh[0], &ustar, &gflux, &tld[0]);

        // Soil
        soiltdm_c(&dedzs[0], &tsoil[0], &zsoil[0], &dzeta, &gflux, &ds, &ni);
        t[0]=t0; //tsoil[0];

        std::cout << i
            << "\t" << dedzm[0]
            << "\t" << dedzt[0]
            << "\t" << zm[0]
            << "\t" << zt[0]
            << "\t" << u[0]
            << "\t" << v[0]
            << "\t" << t[0]
            << "\t" << q[0]
            << "\t" << qi[0]
            << "\t" << e[0]
            << "\t" << ep[0]
            << "\t" << uw[0]
            << "\t" << vw[0]
            << "\t" << wt[0]
            << "\t" << wq[0]
            << "\t" << wqi[0]
            << "\t" << km[0]
            << "\t" << kh[0]
            << "\t" << ustar
            << "\t" << gflux << std::endl;
    }

    std::cout << "Step done\n";

    for ( auto it=u.begin(); it!=u.end(); ++it )
        std::cout << *it << " ";

    std::cout << std::endl;
}
