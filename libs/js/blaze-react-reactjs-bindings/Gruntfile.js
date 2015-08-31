module.exports = function(grunt) {
  grunt.initConfig({
    browserify: {
      lib: {
        files: {
          'lib.js': ['lib.require.js']
        }
      }
    },
    watch: {
      lib: {
        files: 'lib.require.js',
        tasks: ['browserify:lib']
      }
    }
  });
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-browserify');

  grunt.registerTask('default', ['browserify']);
};
