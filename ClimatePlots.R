library(gridExtra)



#########ps###########
p1 <-plot(raster("./SSP1262140/ps.tif"), main='SSP 126 2021-2040 ps')
p2 <-plot(raster("./SSP1264160/ps.tif"), main='SSP 126 2041-2060 ps')
p3 <-plot(raster("./SSP1266180/ps.tif"), main='SSP 126 2061-2080 ps')
p4 <-plot(raster("./SSP1268100/ps.tif"), main='SSP 126 2081-2000 ps')

p5 <-plot(raster("./SSP2452140/ps.tif"), main='SSP 245 2021-2040 ps')
p6 <-plot(raster("./SSP2454160/ps.tif"), main='SSP 245 2041-2060 ps')
p7 <-plot(raster("./SSP2456180/ps.tif"), main='SSP 245 2061-2080 ps')
p8 <-plot(raster("./SSP2458100/ps.tif"), main='SSP 245 2081-2000 ps')

p9 <-plot(raster("./SSP3702140/ps.tif"), main='SSP 370 2021-2040 ps')
p10 <-plot(raster("./SSP3704160/ps.tif"), main='SSP 370 2041-2060 ps')
p11 <-plot(raster("./SSP3706180/ps.tif"), main='SSP 370 2061-2080 ps')
p12 <-plot(raster("./SSP3708100/ps.tif"), main='SSP 370 2081-2000 ps')

p13 <-plot(raster("./SSP5852140/ps.tif"), main='SSP 585 2021-2040 ps')
p14 <-plot(raster("./SSP5854160/ps.tif"), main='SSP 585 2041-2060 ps')
p15 <-plot(raster("./SSP5856180/ps.tif"), main='SSP 585 2061-2080 ps')
p16 <-plot(raster("./SSP5858100/ps.tif"), main='SSP 585 2081-2000 ps')


#########pwm##########
p1 <-plot(raster("./SSP1262140/pwm.tif"), main='SSP 126 2021-2040 pwm')
p2 <-plot(raster("./SSP1264160/pwm.tif"), main='SSP 126 2041-2060 pwm')
p3 <-plot(raster("./SSP1266180/pwm.tif"), main='SSP 126 2061-2080 pwm')
p4 <-plot(raster("./SSP1268100/pwm.tif"), main='SSP 126 2081-2000 pwm')

p5 <-plot(raster("./SSP2452140/pwm.tif"), main='SSP 245 2021-2040 pwm')
p6 <-plot(raster("./SSP2454160/pwm.tif"), main='SSP 245 2041-2060 pwm')
p7 <-plot(raster("./SSP2456180/pwm.tif"), main='SSP 245 2061-2080 pwm')
p8 <-plot(raster("./SSP2458100/pwm.tif"), main='SSP 245 2081-2000 pwm')

p9 <-plot(raster("./SSP3702140/pwm.tif"), main='SSP 370 2021-2040 pwm')
p10 <-plot(raster("./SSP3704160/pwm.tif"), main='SSP 370 2041-2060 pwm')
p11 <-plot(raster("./SSP3706180/pwm.tif"), main='SSP 370 2061-2080 pwm')
p12 <-plot(raster("./SSP3708100/pwm.tif"), main='SSP 370 2081-2000 pwm')

p13 <-plot(raster("./SSP5852140/pwm.tif"), main='SSP 585 2021-2040 pwm')
p14 <-plot(raster("./SSP5854160/pwm.tif"), main='SSP 585 2041-2060 pwm')
p15 <-plot(raster("./SSP5856180/pwm.tif"), main='SSP 585 2061-2080 pwm')
p16 <-plot(raster("./SSP5858100/pwm.tif"), main='SSP 585 2081-2000 pwm')














