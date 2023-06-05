# Wave Function Collapse in ClojureScript

A demo repository for my [blog post][1] about the wave function collapse algorithm.

This [live demo][2] page runs the code from this repository.
You can upload your own images, or choose from predefined ones at the bottom of the page.
There's also a built-in tile-based editor, that can be used to partially draw a picture, and generate the remaining image, or to edit an existing picture, and generate the erased parts:

![demo][3]

## Build

Install `npm` and execute the following command in the project root:

    npx shadow-cljs release app

After that open `index.html` in a browser.

[1]: https://andreyor.st/posts/2022-05-10-wave-function-collapse-algorithm-in-clojurescript
[2]: https://andreyor.st/wave-function-collapse
[3]: https://user-images.githubusercontent.com/19470159/169225363-30f52ae4-0c72-48e2-b8ab-14a61bf3dd78.gif
