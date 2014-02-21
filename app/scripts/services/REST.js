'use strict';

angular.module('blogApp')
  .factory('REST', function ($http) {
    // Service logic
    // ...

    var rest = {
      getPosts: function(cb) {
        $http({
          data: {
            type: 'post'
          }, 
          method: 'POST',
          url: '/rest/view/node'
        })
        .success(function(data){
          console.log('successful post');
          console.log(data);
          cb(data);
        });
      },
      createNode: function(data, cb) {
        console.log(data);
        $http({
          data: data, 
          method: 'POST',
          url: '/rest/create/node'
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
      createNode: function(data, cb) {
        rest.createNode(data,cb);
      },
      createComment: function(data, cb) {
        rest.createComment(data,cb);
      }
    };
  });
