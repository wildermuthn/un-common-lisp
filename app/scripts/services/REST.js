'use strict';

angular.module('blogApp')
  .factory('REST', function ($http) {
    // Service logic
    // ...

    var rest = {
      getPosts: function(cb) {
        $http.get('/rest/all-posts')
        .success(cb);
      },
      createPost: function(data, cb) {
        console.log(data);
        $http({
          data: data, 
          method: 'POST',
          url: '/rest/create-post'
        })
        .success(cb);
      },
      createComment: function(data, cb) {
        console.log(data);
        $http({
          data: data, 
          method: 'POST',
          url: '/rest/create-comment'
        })
        .success(cb);
      }
    };

    // Public API here
    return {
      getPosts: function(cb) {
        rest.getPosts(cb);
      },
      createPost: function(data, cb) {
        rest.createPost(data,cb);
      },
      createComment: function(data, cb) {
        rest.createComment(data,cb);
      }
    };
  });
