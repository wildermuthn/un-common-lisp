'use strict';

angular.module('blogApp')
.factory('Configs', function ($http, $rootScope) {

  return {
    tinyConfigs: {
      options1 : {
        theme: "modern",
        menubar: false,
        toolbar_items_size: 'small',
        toolbar1: "code | styleselect | bold italic | alignleft aligncenter alignright alignjustify | bullist numlist outdent indent | link image preview",
        image_advtab: true,
        width: "100%",
        autoresize_max_height: '500',
        plugins: 'code'
      },
      options2 : {
        theme: "modern",
        menubar: false,
        toolbar_items_size: 'small',
        toolbar1: "bold italic",
        image_advtab: true,
        width: "100%",
        autoresize_max_height: '500'
      }
    }
  }

});
