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
       //alert("FancyWebSocket#open");
    });

    server.bind('message', function(data) {
      service.callback(data);  
    });

    service.server = server;
  }

  service.send_message = function(message) {
    var obj = {message: message, user: 'masayuki'};
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
    ChatService.connect(uri);
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
