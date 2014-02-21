'use strict';

angular.module('blogApp')
  .controller('PostCtrl', function ($scope, REST, $location, Configs) {
    $scope.location = $location;
    $scope.postHTML = '';
    $scope.configs = Configs.tinyConfigs;
    console.log($scope.configs);
    $scope.submit = function() {
      REST.createPost(
       {
         title: $scope.title,
         author: 'Nate Wildermuth',
         postText: $scope.text,
         summary: $scope.summary,
       }, 
       function(result) {
        $location.path('/');
        console.log(result);
      });
    }
  });
