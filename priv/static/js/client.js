var app = angular.module('app', ['ngSanitize']);

app.directive('ngEnter', function() {
  return function (scope, element, attrs) {
    element.bind("keyup", function (event) {
      if (event.which == 13 && event.shiftKey) {
        scope.$apply(function(){
          scope.$eval(attrs.ngEnter);
        });
        event.preventDefault();
      }
    });
  };
});
 
app.factory('ChatService', function() {
  var service = {};
 
  service.connect = function(uri) {
    if(service.server) { return; }
 
    var server = new FancyWebSocket(uri);
    server.bind('open', function() {
      var obj = {user: sessionStorage.getItem("name")};
      service.server.send('join', obj);
    });

    server.bind('message', function(data) {
      service.callback(data);  
    });

    service.server = server;
  }

  service.send_message = function(message) {
    var obj = {message: message, user: sessionStorage.getItem("name")};
    service.server.send('send_message', obj);
  }

  service.subscribe = function(callback) {
    service.callback = callback;
  }

  return service;
});

app.filter('sanitize', function($sanitize) {
  return function(input) {
    return $sanitize(input);
  }
});    

app.filter('convert_linefeed', function() {
  return function(input) {
    return input.replace(/&#10;/, '<br/>');
  }
});

function ChatCtrl($scope, $sanitize, ChatService) {
  $scope.messages = [];

  ChatService.subscribe(function(message) {
    $scope.messages.unshift(message);
    $scope.$apply();
  });

  $scope.connect = function(uri) {
    $scope.uri = uri;
    var storage = sessionStorage;
    var name = storage.getItem('name');
    if(!name) {
      $('#login_dialog').on('shown', function () {
        $('#profile_name').focus();
      });
      $('#login_dialog').modal('show');
    } else {
      ChatService.connect(uri);
    }
  }
 
  $scope.send_message = function(message) {
    var content = chomp(message);
    ChatService.send_message(content);   
  }

  var chomp = function(str) {
    if(str == undefined || str == null) {
      return str;
    }
    return str.replace(/(\n|\r)+$/, '');
  }
}

function LoginCtrl($scope, ChatService) {
  $scope.save_change = function() {
    var storage = sessionStorage;
    storage.setItem('name', $scope.name);
    $('#login_dialog').modal('hide');
    ChatService.connect($scope.uri);
  }
}
