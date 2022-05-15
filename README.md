# Wave Function Collapse in ClojureScript

A demo repository for my [blog post][1] about the wave function collapse algorithm.

This [live demo][2] page runs the code from this repository.
You can upload your own images, or choose from predefined ones at the bottom of the page.
There's also a built-in tile-based editor, that can be used to partially draw a picture, and generate the remaining image, or to edit an existing picture, and generate the erased parts:

![demo](/uploads/34fdefd5b0a02109d3fcbe46ec4c73a3/demo.gif)

The image later can be scrolled using arrow keys, automatically generating the image in given direction.

## Build

Install `npm` and execute the following command in the project root:

    npx shadow-cljs release app

After that open `index.html` in a browser.

[1]: https://andreyorst.gitlab.io/2022-05-10-wave-function-collapse-algorithm-in-clojurescript
[2]: https://andreyorst.gitlab.io/wave-function-collapse
