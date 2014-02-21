'use strict';

angular.module('blogApp')
  .controller('MainCtrl', function ($scope, REST, $location, Configs) {
    $scope.configs = Configs.tinyConfigs;
    $scope.comments = [];
    $scope.location = $location;
    $scope.posts = {};
    REST.getPosts(function(data) {
      for (var i in data) {
        if (data[i].comments == undefined) data[i].comments = [];
        $scope.comments[data[i].id] = {};
        data[i].postTime = moment.unix(data[i].postTime).format("MMM Do YYYY");
      }
      console.log(data);
      $scope.posts = data;
      setTimeout(function() {
        hljs.initHighlightingOnLoad();      
      },1000);
    });
    $scope.submit = function(postRefId) {
      console.log(postRefId);
      var data = {
         postRef: postRefId, 
         postText: $scope.comments[postRefId].text,
      };
      if ($scope.comments[postRefId].author != '' && $scope.comments[postRefId].author != undefined) {
        data.author = $scope.comments[postRefId].author
      }
      else {
        data.author = 'Anonymous';
      }
      REST.createComment(data, function(result) {
        console.log(result);
        location.reload();
      });
    }
  });
