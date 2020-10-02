/*
Copyright The Infusion copyright holders
See the AUTHORS.md file at the top-level directory of this distribution and at
https://github.com/fluid-project/infusion/raw/master/AUTHORS.md.

Licensed under the Educational Community License (ECL), Version 2.0 or the New
BSD license. You may not use this file except in compliance with one these
Licenses.

You may obtain a copy of the ECL 2.0 License and BSD License at
https://github.com/fluid-project/infusion/raw/master/Infusion-LICENSE.txt
*/

/* eslint-env node */
"use strict";

var _ = require("lodash");
var execSync = require("child_process").execSync;

/**
 * Returns a string result from executing a supplied command.
 * The command is executed synchronosly.
 *
 * @param {String} command - the command to execute
 * @param {Object} options - optional arguments "verbose" will output a full stack trace when an error occurs,
 *                           "defaultValue" will set the default return value, useful in case of errors or when a result may be an empty string.
 * @return {String} - returns a string representation of the result of the command or the defaultValue.
 */
var getFromExec = function (command, options) {
    var result = options.defaultValue;
    var stderr = options.verbose ? "pipe" : "ignore";

    try {
        result = execSync(command, {stdio: ["pipe", "pipe", stderr]});
    } catch (e) {
        if (options.verbose) {
            console.log("Error executing command: " + command);
            console.log(e.stack);
        }
    }
    return result;
};

/**
 * Adds '.min' convention in front of the first period of a filename string
 * Example results:
 * infusion-all.js -> infusion-all.min.js
 * infusion-all.js.map -> infusion-all.min.js.map
 * @param {String} fileName - filename string to add '.min' to
 * @return {String} The modified filename string.
 */
var addMin = function (fileName) {
    var segs = fileName.split(".");
    var min = "min";

    if (segs[0] && segs.indexOf(min) < 0 ) {
        segs.splice(1, 0, min);
    }
    return segs.join(".");
};

/**
 * Rename function for grunt file tasks for  adding ".min" convention
 * to filename string; won't do anything to strings that already
 * include ".min"
 * @param {String} dest - supplied by Grunt task, see http://gruntjs.com/configuring-tasks#the-rename-property
 * @param {String} src - supplied by Grunt task, see http://gruntjs.com/configuring-tasks#the-rename-property\
 * @return {String} - The minified version of the original filename.
*/
var addMinifyToFilename = function (dest, src) {
    return dest + addMin(src);
};

module.exports = function (grunt) {

    var setBuildSettings = function (settings) {
        grunt.config.set("buildSettings", {}); // delete previous settings
        _.forEach(settings, function (value, setting) {
            var settingPath = ["buildSettings", setting].join(".");
            grunt.config.set(settingPath, value);
        });
    };

    /** Verifies that directory contains the files in fileList and
    * returns a report for further processing
    * @param {Array} fileList - array of string filenames to check, including full directory paths
    * @return {Object} A report structure for further processing.
    */
    var verifyFiles = function (fileList) {
        var report = {
            fileList: {},
            missingFiles: 0,
            expectedFiles: fileList.length
        };
        _.forEach(fileList, function (fileName) {
            var fileExists = grunt.file.exists(fileName);
            if (!fileExists) {
                report.missingFiles = report.missingFiles + 1;
            }
            report.fileList[fileName] = {"present": fileExists};
        });

        return report;
    };

    /** Displays a file verification report generated by verifyFiles
    * @param {Object} report - the report to display
    */
    var displayVerifyFilesReport = function (report) {
        _.forEach(report.fileList, function (value, fileName) {
            var fileExists = value.present;
            if (fileExists) {
                grunt.log.oklns(fileName + " - ✓ Present".green);
            } else {
                grunt.log.errorlns(fileName + " - ✗ Missing".red);
            }
        });
    };

    /** Processes a file verification report, and fails the build if
    * any files are missing
    * @param {Object} report - the report to process
    */
    var processVerifyFilesReport = function (report) {
        if (report.missingFiles > 0) {
            grunt.log.error("Verification failed".red);
            grunt.fail.fatal(report.missingFiles + " out of " + report.expectedFiles + " expected files not found");
        } else {
            grunt.log.ok("Verification passed".green);
            grunt.log.oklns(report.expectedFiles + " out of " + report.expectedFiles + " expected files were present");
        }
    };

    /** Common function for use by verifyDistFiles task
     *
     *
     * @param {String} message - message to be displayed when running the verify files tasks
     * @param {Array} expectedFilenames - array of expected file names
     * @param {String} fileDir - base directory expected to contain files
     */
    var verifyFilesTaskFunc = function (message, expectedFilenames) {
        grunt.log.subhead(message);
        var report = verifyFiles(expectedFilenames);
        displayVerifyFilesReport(report);
        processVerifyFilesReport(report);
    };

    var verifyFilesListCSSRenameFunc = function (dest, src) {
        var replaced = dest + src.replace("styl", "css");
        return replaced;
    };

    var verifyFilesListMinifiedCSSRenameFunc = function (dest, src) {
        var replaced = dest + src.replace("styl", "min.css");
        return replaced;
    };

    /** verifyFilesListFunc for building a file list of all expected distribution .js and .map files
     * @return {Array} expectedFileNames - the array of expected file names (strings)
     */
    var verifyFilesListJSDistributions = function () {
        var distributions = grunt.config.get("distributions");
        var expectedFilenames = _.flatten(_.map(distributions, function (value, distribution) {
            var jsFilename = "infusion-" + distribution + ".js";
            var mapFilename = jsFilename + ".map";
            var distDirectory = "dist/";
            return [distDirectory + jsFilename, distDirectory + mapFilename];
        }));
        return expectedFilenames;
    };

    // Project configuration.
    grunt.initConfig({
        pkg: grunt.file.readJSON("package.json"),
        revision: getFromExec("git rev-parse --verify --short HEAD", {defaultValue: "Unknown revision, not within a git repository"}),
        branch: getFromExec("git rev-parse --abbrev-ref HEAD", {defaultValue: "Unknown branch, not within a git repository"}),
        allBuildName: "<%= pkg.name %>-all",
        buildSettings: {}, // set by the build tasks
        customBuildName: "<%= pkg.name %>-<%= buildSettings.name %>",
        banner: "/*!\n <%= pkg.name %> - v<%= pkg.version %>\n <%= grunt.template.today('dddd, mmmm dS, yyyy, h:MM:ss TT') %>\n branch: <%= branch %> revision: <%= revision %>*/\n",
        clean: {
            build: "build",
            products: "products",
            stylus: ["src/components/switch/css/*.css", "src/framework/preferences/css/*.css"],
            stylusDist: "dist/assets/**/stylus", // removes the empty stylus directory from the distribution
            ciArtifacts: ["*.tap"],
            dist: "dist",
            postBuild: {
                files: [{}]
            },
            dependencies: [
                "src/lib/hypher",
                "src/lib/jquery/core",
                "src/lib/jquery/plugins",
                "src/lib/jquery/ui/js",
                "src/lib/normalize",
                "src/lib/open-dyslexic",
                "src/lib/opensans",
                "src/lib/roboto",
                "src/lib/url-polyfill",
                "tests/lib/jquery-simulate",
                "tests/lib/mockjax",
                "tests/lib/sinon"
            ]
        },
        copy: {
            all: {
                files: [{
                    expand: true,
                    src: ["src/**", "tests/**", "demos/**", "examples/**"],
                    dest: "build/"
                }]
            },
            custom: {
                files: [{
                    expand: true,
                    src: "<%= modulefiles.custom.output.dirs %>",
                    dest: "build/"
                }]
            },
            necessities: {
                files: [{
                    src: ["README.*", "ReleaseNotes.*", "Infusion-LICENSE.*"],
                    dest: "build/"
                }, {
                    // The jQuery license file needs to be copied explicitly since
                    // "src/lib/jQuery" directory contains several jQuery modules
                    // that have individual dependencies.json files.
                    src: "src/lib/jQuery/jQuery-LICENSE.txt",
                    dest: "build/lib/jQuery/jQuery-LICENSE.txt",
                    filter: function () {
                        return grunt.file.exists("build/lib/jQuery/");
                    }
                }]
            },
            distJS: {
                files: [{
                    expand: true,
                    cwd: "build/",
                    src: "<%= allBuildName %>.*",
                    dest: "dist/",
                    rename: function (dest, src) {
                        return grunt.config.get("buildSettings.compress") ? addMinifyToFilename(dest, src) : dest + src;
                    }
                }, {
                    expand: true,
                    cwd: "build/",
                    src: "<%= customBuildName %>.*",
                    dest: "dist/",
                    rename: function (dest, src) {
                        return grunt.config.get("buildSettings.compress") ? addMinifyToFilename(dest, src, "js") : dest + src;
                    }
                }]
            },
            distAssets: {
                files: [{
                    expand: true,
                    cwd: "build/",
                    src: ["src/framework/preferences/fonts/**", "src/framework/preferences/images/**", "src/lib/open-dyslexic/**", "src/lib/opensans/**"],
                    dest: "dist/assets/"
                }]
            },
            dependencies: {
                files: [{
                    src: "node_modules/hypher/dist/jquery.hypher.js",
                    dest: "src/lib/hypher/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/@fluid-project/hyphenation-patterns/dist/browser/*",
                    dest: "src/lib/hypher/patterns/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/jquery/dist/jquery.js",
                    dest: "src/lib/jquery/core/js/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/jquery-mockjax/dist/jquery.mockjax.js",
                    dest: "tests/lib/mockjax/js/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/jquery.scrollto/jquery.scrollTo.js",
                    dest: "src/lib/jquery/plugins/scrollTo/js/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/jquery-simulate/jquery.simulate.js",
                    dest: "tests/lib/jquery-simulate/js/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/jquery-ui-touch-punch/jquery.ui.touch-punch.js",
                    dest: "src/lib/jquery/plugins/touchPunch/js/",
                    expand: true,
                    flatten: true
                }, {
                    src: [
                        "node_modules/jquery-ui/themes/base/images/*.png"
                    ],
                    dest: "src/lib/jquery/ui/css/default-theme/images",
                    expand: true,
                    flatten: true
                }, {
                    src: [
                        "node_modules/jquery-ui/themes/base/core.css",
                        "node_modules/jquery-ui/themes/base/theme.css",
                        "node_modules/jquery-ui/themes/base/tooltip.css"
                    ],
                    dest: "src/lib/jquery/ui/css/default-theme/",
                    expand: true,
                    flatten: true
                }, {
                    src: [
                        "node_modules/jquery-ui/ui/version.js",
                        "node_modules/jquery-ui/ui/widget.js",
                        "node_modules/jquery-ui/ui/plugin.js",
                        "node_modules/jquery-ui/ui/safe-active-element.js",
                        "node_modules/jquery-ui/ui/safe-blur.js",
                        "node_modules/jquery-ui/ui/position.js",
                        "node_modules/jquery-ui/ui/data.js",
                        "node_modules/jquery-ui/ui/keycode.js",
                        "node_modules/jquery-ui/ui/scroll-parent.js",
                        "node_modules/jquery-ui/ui/unique-id.js",
                        "node_modules/jquery-ui/ui/widgets/mouse.js",
                        "node_modules/jquery-ui/ui/widgets/draggable.js",
                        "node_modules/jquery-ui/ui/widgets/tooltip.js"
                    ],
                    dest: "src/lib/jquery/ui/js/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/normalize.css/normalize.css",
                    dest: "src/lib/normalize/css/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/opensans-webkit/fonts/OpenSans-*.woff",
                    dest: "src/lib/opensans/fonts/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/open-dyslexic/woff/OpenDyslexic-*.woff",
                    dest: "src/lib/open-dyslexic/fonts/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/roboto-fontface/fonts/roboto-slab/Roboto-Slab-*.woff",
                    dest: "src/lib/roboto/fonts/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/sinon/pkg/sinon.js",
                    dest: "tests/lib/sinon/js/",
                    expand: true,
                    flatten: true
                }, {
                    src: "node_modules/url-polyfill/url-polyfill.js",
                    dest: "src/lib/url-polyfill/js/",
                    expand: true,
                    flatten: true
                }]
            }
        },
        uglify: {
            options: {
                banner: "<%= banner %>",
                mangle: false,
                sourceMap: true,
                sourceMapIncludeSources: true
            },
            all: {
                files: [{
                    src: "<%= modulefiles.all.output.files %>",
                    dest: "./build/<%= allBuildName %>.js"
                }]
            },
            custom: {
                files: [{
                    src: "<%= modulefiles.custom.output.files %>",
                    dest: "./build/<%= customBuildName %>.js"
                }]
            }
        },
        modulefiles: {
            all: {
                src: ["src/**/*Dependencies.json"]
            },
            custom: {
                options: {
                    exclude: "<%= buildSettings.exclude %>",
                    include: "<%= buildSettings.include %>"
                },
                src: ["src/**/*Dependencies.json"]
            }
        },
        map: {
            // append "/**" to the end of all of all of
            // directory paths for copy:custom to ensure that
            // all of the subdirectories and files are copied over
            copyDirs: {
                files: "<%= copy.custom.files %>",
                prop: "copy.custom.files.0.src",
                fn: function (str) {
                    return str + "/**";
                }
            },
            postBuildClean: {
                files: "<%= clean.postBuild.files %>",
                prop: "clean.postBuild.files.0.src",
                fn: function (str) {
                    var buildPath = "build/";
                    return str.startsWith(buildPath) ? str : buildPath + str;
                }
            }
        },
        // Still need the concat task as uglify does not honour the {compress: false} option
        // see: https://github.com/mishoo/UglifyJS2/issues/696
        concat: {
            options: {
                separator: ";\n",
                banner: "<%= banner %>",
                sourceMap: true
            },
            all: {
                nonull: true,
                cwd: "./build/", // Src matches are relative to this path.
                src: "<%= modulefiles.all.output.files %>",
                dest: "./build/<%= allBuildName %>.js"
            },
            custom: {
                nonull: true,
                cwd: "./build/", // Src matches are relative to this path.
                src: "<%= modulefiles.custom.output.files %>",
                dest: "./build/<%= customBuildName %>.js"
            }
        },
        compress: {
            all: {
                options: {
                    archive: "products/<%= allBuildName %>-<%= pkg.version %>.zip"
                },
                files: [{
                    expand: true,     // Enable dynamic expansion.
                    cwd: "./build/",      // Src matches are relative to this path.
                    src: ["**/*"], // Actual pattern(s) to match.
                    dest: "./infusion"   // Destination path prefix in the zip package
                }]
            },
            custom: {
                options: {
                    archive: "products/<%= customBuildName %>-<%= pkg.version %>.zip"
                },
                files: "<%= compress.all.files %>"
            }
        },
        lintAll: {
            sources: {
                md: [ "*.md", "!./src/**/lib/**/*.md", "!./demos/**/lib/**/*.md", "!./tests/**/lib/**/*.md"],
                js: ["*.js", "!./src/**/lib/**/*.js", "!./demos/**/lib/**/*.js", "!./dist/**/*.js", "!./tests/**/lib/**/*.js", "!./tests/**/infusion-1.5.js"],
                json: ["*.json", "./.nycrc", "./src/thirdPartyDependencies.json", "!./src/lib/**/*.json", "!./dist/**/*.json"],
                other: ["./.*"]
            }
        },
        stylus: {
            compile: {
                options: {
                    compress: "<%= buildSettings.compress %>",
                    relativeDest: ".."
                },
                files: [{
                    expand: true,
                    src: ["src/**/css/stylus/*.styl"],
                    ext: ".css"
                }]
            },
            dist: {
                options: {
                    compress: "<%= buildSettings.compress %>",
                    relativeDest: ".."
                },
                files: [{
                    expand: true,
                    src: ["src/**/css/stylus/*.styl"],
                    ext: "<% buildSettings.compress ? print('.min.css') : print('.css') %>",
                    dest: "dist/assets/"
                }]
            }
        },
        // grunt-contrib-watch task to watch and rebuild stylus files
        // automatically when doing stylus development
        watch: {
            buildStylus: {
                files: ["src/**/css/stylus/*.styl", "src/**/css/stylus/utils/*.styl"],
                tasks: "buildStylus"
            }
        },
        distributions:
        {
            "all": {},
            "all.min": {
                options: {
                    compress: true
                }
            },
            "all-no-jquery": {
                options: {
                    exclude: "jQuery, jQueryUI"
                }
            },
            "all-no-jquery.min": {
                options: {
                    exclude: "jQuery, jQueryUI",
                    compress: true
                }
            },
            "framework": {
                options: {
                    include: "framework"
                }
            },
            "framework.min": {
                options: {
                    include: "framework",
                    compress: true
                }
            },
            "framework-no-jquery": {
                options: {
                    include: "framework",
                    exclude: "jQuery, jQueryUI"
                }
            },
            "framework-no-jquery.min": {
                options: {
                    include: "framework",
                    exclude: "jQuery, jQueryUI",
                    compress: true
                }
            },
            "uio": {
                options: {
                    include: "uiOptions"
                }
            },
            "uio.min": {
                options: {
                    include: "uiOptions",
                    compress: true
                }
            },
            "uio-no-jquery": {
                options: {
                    include: "uiOptions",
                    exclude: "jQuery, jQueryUI"
                }
            },
            "uio-no-jquery.min": {
                options: {
                    include: "uiOptions",
                    exclude: "jQuery, jQueryUI",
                    compress: true
                }
            }
        },
        verifyDistFiles: {
            fonts: {
                files: [{
                    expand: true,
                    src: ["*.woff"],
                    cwd: "src/lib/opensans/fonts/",
                    dest: "dist/assets/src/lib/opensans/fonts/"
                }, {
                    expand: true,
                    src: ["*.woff"],
                    cwd: "src/lib/open-dyslexic/fonts/",
                    dest: "dist/assets/src/lib/open-dyslexic/fonts/"
                }, {
                    expand: true,
                    src: ["*.woff"],
                    cwd: "src/framework/preferences/fonts/",
                    dest: "dist/assets/src/framework/preferences/fonts/"
                }]
            },
            js: {
                verifyFilesListFunc: verifyFilesListJSDistributions
            },
            css: {
                files: [
                    {
                        expand: true,
                        src: ["*.styl"],
                        cwd: "src/framework/preferences/css/stylus/",
                        dest: "dist/assets/src/framework/preferences/css/",
                        rename: verifyFilesListCSSRenameFunc
                    },
                    {
                        expand: true,
                        src: ["*.styl"],
                        cwd: "src/framework/preferences/css/stylus/",
                        dest: "dist/assets/src/framework/preferences/css/",
                        rename: verifyFilesListMinifiedCSSRenameFunc
                    },
                    {
                        expand: true,
                        src: ["*.styl"],
                        cwd: "src/components/switch/css/stylus/",
                        dest: "dist/assets/src/components/switch/css/",
                        rename: verifyFilesListCSSRenameFunc
                    },
                    {
                        expand: true,
                        src: ["*.styl"],
                        cwd: "src/components/switch/css/stylus/",
                        dest: "dist/assets/src/components/switch/css/",
                        rename: verifyFilesListMinifiedCSSRenameFunc
                    }
                ]
            }
        }
    });

    // Load the plugins:
    grunt.loadNpmTasks("grunt-contrib-uglify");
    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-contrib-copy");
    grunt.loadNpmTasks("grunt-contrib-concat");
    grunt.loadNpmTasks("grunt-contrib-compress");
    grunt.loadNpmTasks("grunt-modulefiles");
    grunt.loadNpmTasks("grunt-contrib-stylus");
    grunt.loadNpmTasks("grunt-contrib-watch");
    grunt.loadNpmTasks("fluid-grunt-lint-all");

    // Custom tasks:

    // Simple task for transforming a property
    grunt.registerMultiTask("map", "a task wrapper around the map function from lodash", function () {
        var transformed = _.map(this.filesSrc, this.data.fn);
        grunt.config.set(this.data.prop, transformed);
    });

    grunt.registerTask("pathMap", "Triggers the map task for the specified build target", function (target) {
        grunt.task.run("map:postBuildClean");
        if (target === "custom") {
            grunt.task.run("map:copyDirs");
        }
    });

    grunt.registerTask("setPostBuildCleanUp", "Sets the file source for post build cleanup", function (target) {
        grunt.config.set("clean.postBuild.files.0.src", "<%= modulefiles." + target + ".output.files %>");
    });

    // Task for organizing the build
    grunt.registerTask("build", "Generates a minified or source distribution for the specified build target", function (target) {
        target = target || "all";
        setBuildSettings({
            name: grunt.option("name") || "custom",
            exclude: grunt.option("exclude"),
            include: grunt.option("include"),
            compress: !grunt.option("source"),
            target: target
        });
        var concatTask = grunt.config.get("buildSettings.compress") ? "uglify:" : "concat:";
        var tasks = [
            "clean",
            "copy:dependencies",
            "lint",
            "stylus:compile",
            "modulefiles:" + target,
            "setPostBuildCleanUp:" + target,
            "pathMap:" + target,
            "copy:" + target,
            "copy:necessities",
            concatTask + target,
            "compress:" + target,
            "clean:postBuild"
        ];
        grunt.task.run(tasks);
    });

    grunt.registerMultiTask("distributions", "Enables a project to split its files into a set of modules. A module's information is stored in a json file containing a name for the module, the files it contains, and other modules it depends on. The module files can then be accumulated into various configurations of included and excluded modules, which can be fed into other plugins (e.g. grunt-contrib-concat) for packaging.", function () {
        // Merge task-specific and/or target-specific options with these defaults.
        var options = this.options({
            name: this.target,
            source: true,
            target: "all",
            compress: false
        });

        if (options.exclude || options.include) {
            options.target = "custom";
        }

        setBuildSettings(options);

        var concatTask = options.compress ? "uglify:" : "concat:";
        var tasks = [
            "cleanForDist",
            "stylus:dist",
            "modulefiles:" + options.target,
            "pathMap:" + options.target,
            "copy:" + options.target,
            "copy:necessities",
            concatTask + options.target,
            "copy:distJS",
            "copy:distAssets"
        ];
        grunt.task.run(tasks);
    });

    grunt.registerTask("buildDists", "Tasks to run before publishing to NPM", function (target) {
        var tasks = [
            "clean",
            "copy:dependencies",
            "lint",
            "distributions" + ( target ? ":" + target : "" ),
            "cleanForDist",
            "verifyDistFiles",
            "buildStylus" // put back stylus files needed for development
        ];
        grunt.task.run(tasks);
    });

    grunt.registerMultiTask("verifyDistFiles", "Verify distribution files", function () {
        var message = "Verifying all \"" + this.target + "\" files are in /dist";
        var expectedFiles;

        // If the target uses the standard Grunt file options
        if (this.files.length > 1) {
            expectedFiles = _.map(this.files, function (file) {
                return file.dest;
            });

        // If the target has specified a custom function for generating the list of files to check
        } else if (this.data.verifyFilesListFunc) {
            if (typeof this.data.verifyFilesListFunc === "function") {
                expectedFiles = this.data.verifyFilesListFunc();
            }
        } else {
            grunt.log("Task target must either use standard Grunt 'file' options to produce a set of files to check or specify a 'verifyFilesListFunc' returning an array of file paths to check");
        }
        verifyFilesTaskFunc(message, expectedFiles);
    });

    grunt.registerTask("cleanForDist", ["clean:build", "clean:products", "clean:stylusDist", "clean:ciArtifacts"]);
    grunt.registerTask("buildStylus", ["clean:stylus", "stylus:compile"]);

    grunt.registerTask("default", ["build:all"]);
    grunt.registerTask("custom", ["build:custom"]);

    grunt.registerTask("lint", "Perform all standard lint checks.", ["lint-all"]);
    grunt.registerTask("loadDependencies", "Load lib files from node_modules", ["clean:dependencies", "copy:dependencies"]);
};
