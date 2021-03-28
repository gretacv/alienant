# alienant WIP
This package aims at identifying climatic and human connections between Antarctic ACBRs producing several networks. Each function is a step in the process. 
- `resample_hfp` Resample the human foot print data
- `prep_hfp` Prepare the human foot print data
- `prep_clim` Prepare the climatic data
- `clim_net()` Climatic network
- `hum_net()` Human network
- `exdet_acbrs()` Calculate Exdet between ACBRs
- `classification_exdet()` Classification of the climatic similarity using the proportion of climate analogues an ACBR has of another and viceversa


## Requirements
`ArcPy` for python.
`ecospat`
`igraph`
`raster`
`reshape2`
`reticulate`
