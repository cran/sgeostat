fit.variogram<-function(model="exponential", ...){
        switch(model,
               exponential = fit.exponential(...),
               gaussian = fit.gaussian(...),
               wave = fit.wave(...),
               linear = fit.linear(...),
               spherical = fit.spherical(...)
		)
}
