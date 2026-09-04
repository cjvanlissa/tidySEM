# Format numeric columns Formats the numeric columns of a data.frame, to round to a specific number of digits.

Format numeric columns Formats the numeric columns of a data.frame, to
round to a specific number of digits.

## Usage

``` r
format_numeric(x, digits = 2)
```

## Arguments

- x:

  A data.frame.

- digits:

  The desired number of digits.

## Author

Caspar J. van Lissa

## Examples

``` r
dat <- mtcars
format_numeric(dat, 1)
#>                      mpg cyl  disp    hp drat  wt qsec  vs  am gear carb
#> Mazda RX4           21.0 6.0 160.0 110.0  3.9 2.6 16.5 0.0 1.0  4.0  4.0
#> Mazda RX4 Wag       21.0 6.0 160.0 110.0  3.9 2.9 17.0 0.0 1.0  4.0  4.0
#> Datsun 710          22.8 4.0 108.0  93.0  3.9 2.3 18.6 1.0 1.0  4.0  1.0
#> Hornet 4 Drive      21.4 6.0 258.0 110.0  3.1 3.2 19.4 1.0 0.0  3.0  1.0
#> Hornet Sportabout   18.7 8.0 360.0 175.0  3.1 3.4 17.0 0.0 0.0  3.0  2.0
#> Valiant             18.1 6.0 225.0 105.0  2.8 3.5 20.2 1.0 0.0  3.0  1.0
#> Duster 360          14.3 8.0 360.0 245.0  3.2 3.6 15.8 0.0 0.0  3.0  4.0
#> Merc 240D           24.4 4.0 146.7  62.0  3.7 3.2 20.0 1.0 0.0  4.0  2.0
#> Merc 230            22.8 4.0 140.8  95.0  3.9 3.1 22.9 1.0 0.0  4.0  2.0
#> Merc 280            19.2 6.0 167.6 123.0  3.9 3.4 18.3 1.0 0.0  4.0  4.0
#> Merc 280C           17.8 6.0 167.6 123.0  3.9 3.4 18.9 1.0 0.0  4.0  4.0
#> Merc 450SE          16.4 8.0 275.8 180.0  3.1 4.1 17.4 0.0 0.0  3.0  3.0
#> Merc 450SL          17.3 8.0 275.8 180.0  3.1 3.7 17.6 0.0 0.0  3.0  3.0
#> Merc 450SLC         15.2 8.0 275.8 180.0  3.1 3.8 18.0 0.0 0.0  3.0  3.0
#> Cadillac Fleetwood  10.4 8.0 472.0 205.0  2.9 5.2 18.0 0.0 0.0  3.0  4.0
#> Lincoln Continental 10.4 8.0 460.0 215.0  3.0 5.4 17.8 0.0 0.0  3.0  4.0
#> Chrysler Imperial   14.7 8.0 440.0 230.0  3.2 5.3 17.4 0.0 0.0  3.0  4.0
#> Fiat 128            32.4 4.0  78.7  66.0  4.1 2.2 19.5 1.0 1.0  4.0  1.0
#> Honda Civic         30.4 4.0  75.7  52.0  4.9 1.6 18.5 1.0 1.0  4.0  2.0
#> Toyota Corolla      33.9 4.0  71.1  65.0  4.2 1.8 19.9 1.0 1.0  4.0  1.0
#> Toyota Corona       21.5 4.0 120.1  97.0  3.7 2.5 20.0 1.0 0.0  3.0  1.0
#> Dodge Challenger    15.5 8.0 318.0 150.0  2.8 3.5 16.9 0.0 0.0  3.0  2.0
#> AMC Javelin         15.2 8.0 304.0 150.0  3.1 3.4 17.3 0.0 0.0  3.0  2.0
#> Camaro Z28          13.3 8.0 350.0 245.0  3.7 3.8 15.4 0.0 0.0  3.0  4.0
#> Pontiac Firebird    19.2 8.0 400.0 175.0  3.1 3.8 17.1 0.0 0.0  3.0  2.0
#> Fiat X1-9           27.3 4.0  79.0  66.0  4.1 1.9 18.9 1.0 1.0  4.0  1.0
#> Porsche 914-2       26.0 4.0 120.3  91.0  4.4 2.1 16.7 0.0 1.0  5.0  2.0
#> Lotus Europa        30.4 4.0  95.1 113.0  3.8 1.5 16.9 1.0 1.0  5.0  2.0
#> Ford Pantera L      15.8 8.0 351.0 264.0  4.2 3.2 14.5 0.0 1.0  5.0  4.0
#> Ferrari Dino        19.7 6.0 145.0 175.0  3.6 2.8 15.5 0.0 1.0  5.0  6.0
#> Maserati Bora       15.0 8.0 301.0 335.0  3.5 3.6 14.6 0.0 1.0  5.0  8.0
#> Volvo 142E          21.4 4.0 121.0 109.0  4.1 2.8 18.6 1.0 1.0  4.0  2.0
```
