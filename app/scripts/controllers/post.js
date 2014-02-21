'use strict';

angular.module('blogApp')
  .controller('PostCtrl', function ($scope, REST, $location, Configs) {
    $scope.location = $location;
    $scope.postHTML = '';
    $scope.configs = Configs.tinyConfigs;
    $scope.submit = function() {
      REST.createNode(
       {
         title: $scope.title,
         type: 'post',
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
