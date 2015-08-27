"use strict"

var gulp = require("gulp"),
    purescript = require("gulp-purescript"),
    webpack = require("webpack-stream");

var sources = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs",
    "test/*.purs"
];
var foreigns = [
    "src/**/*.js",
    "bower_components/purescript-*/src/**/*.js",
    "test/*/*.js"
];

gulp.task("make", function() {
  return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task("docs", function() {
    return purescript.pscDocs({
        src: sources,
        docgen: {
            "Platform": "docs/Platform.md"
        }
    });
});

gulp.task("bundle", ["make"], function() {
  return purescript.pscBundle({
    src: "output/**/*.js",
    output: "tmp/test.js",
    main: "Test.Main"
  });
});

gulp.task("bundle-test", ["bundle"], function() {
  return gulp.src("tmp/test.js")
    .pipe(webpack({
      resolve: { moduleDirectories: ["node_modules"] },
      output: { filename: "test.js" }
    }))
    .pipe(gulp.dest("tmp"));
});

gulp.task("dotpsci", function () {
  return purescript.psci({ src: sources, ffi: foreigns })
    .pipe(gulp.dest("."));
});

gulp.task("default", ["bundle-test", "docs", "dotpsci"]);
