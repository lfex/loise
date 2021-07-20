# Background

## Perlin Noise

Perlin noise is a computer-generated visual effect
developed by Ken Perlin, who won an Academy Award for Technical Achievement for
inventing it. It can be used to simulate elements from nature, and is especially
useful in circumstances where computer memory is limited. (See the complete
[Perlin Wikipedia article](http://en.wikipedia.org/wiki/Perlin_noise).)

## Simplex Noise

Simplex noise, on the other hand, is a method for constructing an n-dimensional
noise function comparable to Perlin noise ("classic" noise) but with a lower
computational overhead, especially in larger dimensions. Ken Perlin designed
the algorithm in 2001 to address the limitations of his classic noise
function, especially in higher dimensions. (See the complete
[Simplex Wikipedia article](http://en.wikipedia.org/wiki/Simplex_noise) for
more.)

## Project Origins

The loise project stated life as a port of the
[Racket noise-generator](https://github.com/jpverkamp/noise) by
[jpverkamp](https://github.com/jpverkamp) to LFE. However, it has undergone
some seriosu refactoring since then, as well as the inclusion of many new
features.
