# zircon-trace-element-plots
Code used to create zircon trace element plots using lanthanoids, U, Yb, & Y

This code is used to generate several trace elements plots for zircon. 
1. The first is a simple U/Yb against Y plot used as a way to distinguish zircons derived from continental material and oceaninc material
2. The second set of plots are scatter plots with a 2d kernel density estimate overlain, with the bandwidth chosen via the botev algorithm implemented in the Provenance package (Vermeesch 2019, doi:10.3390/min9030193 - https://github.com/pvermees/provenance)
    Generated plots are Yb/U as a proxy similar to hafnium (see Verdel et al. 2021, doi:10.1130/ges02300.1), Eu and Ce anomalies, and lambda 1-3 (lanthanoid pattern shape), calculated via BLambdaR (see Anenburg and Williams 2021, doi:10.1007/s11004-021-09959-5, forked version for offline use with large datasets available at https://github.com/jarredclloyd/BLambdaR)
3. The third set of plots are 50 Ma binned box plots of the same values in 2
4. The fourth available plot is violin plots of normalised (CI chondrite, O'Neill 2016, doi:10.1093/petrology/egw047) lanthanoid element concentrations spaced by their Shannon ionic radii (Shannon 1976, doi:10.1107/S0567739476001551)

 Colour values utilise the scico package for colour-blind safe palettes (see Crameri et al. 2020, doi:10.1038/s41467-020-19160-7 and https://github.com/thomasp85/scico), use scico::scico_palette_show() to view available colour palettes in the package. Code can easily be modified to use alternate colour packages, base colours or custom colour mapping.